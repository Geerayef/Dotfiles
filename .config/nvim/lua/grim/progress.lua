-----------------------------------------------
-- LSP progress — functional / functional-logic core
-----------------------------------------------
--
-- Architecture (Elm/TEA): a pure-by-contract transition
--   update : (World, Msg, Env) -> Either<ErrorEnv, World>
-- folded over the stream of `Msg`s delivered by Neovim's event loop. `vim.schedule` is
-- FIFO and never re-entrant, so the event loop *is* the queue: no queue, no flag.
-- The single mutable cell `W` is rebound in exactly one expression (`dispatch`) — the
-- moral equivalent of an `IORef` at Haskell's `main` edge.
--
-- Time is data: delayed work (`settle`, retries) is a `Msg` posted for later and guarded
-- by a per-stream generation counter `gen`. No libuv timers: nothing to leak, stop, or
-- double-close.
--
-- Every Neovim call lives in `ENV`; errors are classified exactly once, at that edge,
-- into `ErrorEnv`. The core never pattern-matches error strings.

---@type GRIM.base.Func
local F = GRIM.base.func
---@type GRIM.base.Seq
local Seq = GRIM.base.seq
---@type GRIM.base.Map
local Map = GRIM.base.map
---@type GRIM.base.Match
local Match = GRIM.base.match
---@type GRIM.base.monad.Maybe
local Maybe = GRIM.base.monad.maybe
---@type GRIM.base.monad.Either
local Either = GRIM.base.monad.either
local notify = GRIM.notify.notify

--- Types ------------------------------------------------------------------------------------------

---@class Config
---@field spinner_frame_repeats integer
---@field done_delay_ms integer
---@field retry_interval_ms integer
---@field window_zindex integer

---An `Env` failure, classified once at the edge.
---@class ErrorEnv
---@field expected boolean # Transient editor state (textlock): skip or retry, never report.
---@field message string

---Lifecycle of a stream's resources. Illegal states are unrepresentable:
---a window implies a buffer and a stack slot; `idle` owns nothing.
---@class Idle
---@field tag "idle"

---@class Ready
---@field tag "ready"
---@field bufnr integer

---@class Showing
---@field tag "showing"
---@field bufnr integer
---@field winid integer # May have been closed externally; check with `live`.
---@field pos integer # Stack slot from the bottom, 1-based; slots form 1..k.

---@alias Phase Idle | Ready | Showing
---@alias PhaseTag "idle" | "ready" | "showing"

---Immutable per-stream record (one per `(client_id, token)` work-done stream).
---@class ProgressState
---@field client_id integer
---@field is_done boolean
---@field spinner_idx integer
---@field gen integer # Bumped on every arm; a `settle` with a stale `gen` is ignored.
---@field message Maybe<string>
---@field phase Phase

---Typed partial update for `with` (a misspelt field is a type error).
---@class ProgressPatch
---@field is_done boolean?
---@field spinner_idx integer?
---@field gen integer?
---@field message Maybe<string>?
---@field phase Phase?

---@alias StatesMap table<string, ProgressState>

---Immutable snapshot; replaced wholesale. The stack size is derived (`stack_size`).
---@class World
---@field states StatesMap

---LSP work-done value as seen by `LspProgress` (runtime grafts `title` onto report/end).
---@class WorkDoneProgressValue
---@field kind "begin" | "report" | "end"
---@field title string?
---@field message string?
---@field percentage integer?
---@field cancellable boolean?

---@alias ProgressAction "reopen" | "reposition" | "open"

---@class MsgProgress
---@field kind "progress"
---@field data vim.event.lspprogress.data

---@class MsgDetach
---@field kind "detach"
---@field client_id integer

---@class MsgResize
---@field kind "resize"

---@class MsgTabEnter
---@field kind "tab_enter"

---@class MsgSettle
---@field kind "settle"
---@field key string
---@field gen integer

---@class MsgPurge
---@field kind "purge"
---@field key string

---Every event the core reacts to, as data (replayable, loggable, testable).
---@alias MSG MsgProgress | MsgDetach | MsgResize | MsgTabEnter | MsgSettle | MsgPurge
---@alias MsgKind "progress" | "detach" | "resize" | "tab_enter" | "settle" | "purge"

---The only impure surface (injected). Effects return `Either<ErrorEnv, _>`; queries
---are plain reads; `post`/`post_later`/`report` close the loop back into `dispatch`.
---@class Env
---@field open_win fun(bufnr: integer, enter: boolean, config: vim.api.keyset.win_config): Either<ErrorEnv, integer>
---@field close_win fun(winid: integer): Either<ErrorEnv, nil>
---@field set_config fun(winid: integer, config: vim.api.keyset.win_config): Either<ErrorEnv, nil>
---@field set_lines fun(bufnr: integer, lines: string[]): Either<ErrorEnv, nil>
---@field create_buf fun(): Either<ErrorEnv, integer>
---@field delete_buf fun(bufnr: integer): Either<ErrorEnv, nil>
---@field win_get_tabpage fun(winid: integer): Either<ErrorEnv, integer>
---@field strdisplaywidth fun(s: string): Either<ErrorEnv, integer>
---@field get_client_by_id fun(id: integer): Maybe<vim.lsp.Client>
---@field win_is_valid fun(winid: integer): boolean
---@field buf_is_valid fun(bufnr: integer): boolean
---@field current_tabpage fun(): integer
---@field lines fun(): integer
---@field cmdheight fun(): integer
---@field columns fun(): integer
---@field border fun(): Maybe<string>
---@field post fun(msg: MSG) # Deliver `msg` on the next event-loop tick.
---@field post_later fun(msg: MSG, ms: integer) # Deliver `msg` after `ms` milliseconds.
---@field report fun(err: ErrorEnv) # Surface an unexpected failure to the user.

--- Constants --------------------------------------------------------------------------------------

---@type Config
local CONFIG = F.immutable({
  spinner_frame_repeats = 2,
  done_delay_ms = 2000,
  retry_interval_ms = 100,
  window_zindex = 30,
})

---@type { spinner: string[], done: string }
local ICONS = F.immutable({
  spinner = GRIM.static.icon.spinner.dots_l,
  done = GRIM.static.icon.ui.check,
})

---@type string[]
local EXPECTED_ERRORS = F.immutable({
  "E11: Invalid in command%-line window",
  "E523: Not allowed here",
  "E565: Not allowed to change",
})

---@type PhaseTag[]
local PHASE_TAGS = F.immutable({ "idle", "ready", "showing" })
---@type ProgressAction[]
local RENDER_ACTIONS = F.immutable({ "reopen", "reposition", "open" })
---@type MsgKind[]
local MSG_KINDS = F.immutable({ "progress", "detach", "resize", "tab_enter", "settle", "purge" })

---@type Idle
local IDLE = F.immutable({ tag = "idle" })
---@type MsgResize
local MSG_RESIZE = F.immutable({ kind = "resize" })
---@type MsgTabEnter
local MSG_TAB_ENTER = F.immutable({ kind = "tab_enter" })

--- Errors -----------------------------------------------------------------------------------------
---Classification (edge) and recovery combinators (core)

---@param message string
---@return boolean
local function is_expected(message)
  return Seq.any(function(e) return string.find(message, e) ~= nil end, EXPECTED_ERRORS)
end

---`xpcall` handler: raw error -> `ErrorEnv`. Unexpected errors keep their traceback.
---@param err any
---@return ErrorEnv
local function classify(err)
  local message = tostring(err)
  local expected = is_expected(message)
  return F.immutable({
    expected = expected,
    message = expected and message or debug.traceback(message, 2),
  })
end

---An internal invariant violation (never expected, always reported).
---@param message string
---@return Left<ErrorEnv>
local function fault(message)
  return Either.left(F.immutable({ expected = false, message = "GRIM.progress: " .. message }))
end

---Value-producing effect: expected error -> `Right(Nothing)` (skipped), else unchanged.
---Replaces `swallow` for `open_win` / `strdisplaywidth`.
---@generic A
---@param e Either<ErrorEnv, A>
---@return Either<ErrorEnv, Maybe<A>>
local function recover(e)
  return Either.or_else(function(err)
    if err.expected then return Either.right(Maybe.nothing()) end
    return Either.left(err)
  end, Either.map(Maybe.of_nil, e))
end

---Unit effect: `Right(true)` = performed, `Right(false)` = skipped (expected error).
---Replaces `swallow`/`swallow_err` for `set_config` / `set_lines` / `close_win`.
---@param e Either<ErrorEnv, any>
---@return Either<ErrorEnv, boolean>
local function attempt(e)
  return Either.or_else(function(err)
    if err.expected then return Either.right(false) end
    return Either.left(err)
  end, Either.map(F.const(true), e))
end

---Continue with a recovered value, or stop with `world` unchanged if it was skipped.
---@generic A
---@param world World
---@param f fun(a: A): Either<ErrorEnv, World>
---@param e Either<ErrorEnv, Maybe<A>>
---@return Either<ErrorEnv, World>
local function proceed(world, f, e)
  return Either.bind(function(m)
    return Maybe.fold(function() return Either.right(world) end, f, m)
  end, e)
end

---Discard an effect's result, keeping `world`.
---@param world World
---@param e Either<ErrorEnv, any>
---@return Either<ErrorEnv, World>
local function after(world, e) return Either.map(F.const(world), e) end

---Release policy: an expected failure re-posts `retry` later and keeps `world`
---(the state still owns its resources); an unexpected one propagates.
---@param world World
---@param retry MSG
---@param env Env
---@param e Either<ErrorEnv, World>
---@return Either<ErrorEnv, World>
local function retrying(world, retry, env, e)
  return Either.or_else(function(err)
    if not err.expected then return Either.left(err) end
    env.post_later(retry, CONFIG.retry_interval_ms)
    return Either.right(world)
  end, e)
end

---Commit `world` even though the follow-up `e` failed: the error is reported and the
---resources already recorded in `world` stay tracked (no leak).
---@param world World
---@param env Env
---@param e Either<ErrorEnv, World>
---@return Either<ErrorEnv, World>
local function salvage(world, env, e)
  return Either.or_else(function(err)
    env.report(err)
    return Either.right(world)
  end, e)
end

--- Records (pure) ---------------------------------------------------------------------------------

---@param bufnr integer
---@return Ready
local function ready(bufnr) return F.immutable({ tag = "ready", bufnr = bufnr }) end

---@param bufnr integer
---@param winid integer
---@param pos integer
---@return Showing
local function showing(bufnr, winid, pos)
  return F.immutable({ tag = "showing", bufnr = bufnr, winid = winid, pos = pos })
end

---@param client_id integer
---@return ProgressState
local function initial(client_id)
  return F.immutable({
    client_id = client_id,
    is_done = false,
    spinner_idx = 0,
    gen = 0,
    message = Maybe.nothing(),
    phase = IDLE,
  })
end

---@param s ProgressState
---@param patch ProgressPatch
---@return ProgressState
local function with(s, patch) return F.immutable(Map.merge(s, patch)) end

---@param states StatesMap
---@return World
local function world_of(states) return F.immutable({ states = F.immutable(states) }) end

---@param world World
---@param key string
---@param s ProgressState
---@return World
local function update_state(world, key, s) return world_of(Map.merge(world.states, { [key] = s })) end

---@param world World
---@param key string
---@return World
local function remove_state(world, key) return world_of(Map.omit({ key }, world.states)) end

---Internal step: the state MUST exist; absence is a reported fault.
---@param world World
---@param key string
---@param f fun(s: ProgressState): Either<ErrorEnv, World>
---@return Either<ErrorEnv, World>
local function with_state(world, key, f)
  return Maybe.fold(
    function() return fault("no state for key " .. key) end,
    f,
    Maybe.of_nil(world.states[key])
  )
end

---Entry point for delayed messages: the state MAY be gone (purged); absence is a no-op.
---@param world World
---@param key string
---@param f fun(s: ProgressState): Either<ErrorEnv, World>
---@return Either<ErrorEnv, World>
local function when_state(world, key, f)
  return Maybe.fold(function() return Either.right(world) end, f, Maybe.of_nil(world.states[key]))
end

---@type fun(tag: PhaseTag, phase: Phase): Maybe<Showing>
local showing_of_tag = Match.total(PHASE_TAGS, {
  idle = function() return Maybe.nothing() end,
  ready = function() return Maybe.nothing() end,
  showing = function(_, p) return Maybe.just(p) end,
})

---@param phase Phase
---@return Maybe<Showing>
local function showing_of(phase) return showing_of_tag(phase.tag, phase) end

---@type fun(tag: PhaseTag, phase: Phase): Maybe<integer>
local bufnr_of_tag = Match.total(PHASE_TAGS, {
  idle = function() return Maybe.nothing() end,
  ready = function(_, p) return Maybe.just(p.bufnr) end,
  showing = function(_, p) return Maybe.just(p.bufnr) end,
})

---@param phase Phase
---@return Maybe<integer>
local function bufnr_of(phase) return bufnr_of_tag(phase.tag, phase) end

---@param phase Phase
---@return Maybe<integer>
local function pos_of(phase)
  return Maybe.map(function(p) return p.pos end, showing_of(phase))
end

---Showing with a window that still exists.
---@param s ProgressState
---@param env Env
---@return Maybe<Showing>
local function live(s, env)
  return Maybe.filter(function(p) return env.win_is_valid(p.winid) end, showing_of(s.phase))
end

---Number of reserved stack slots (derived; replaces the old `stack_size` field).
---@param world World
---@return integer
local function stack_size(world)
  return Map.fold_with_key(function(n, s)
    if Maybe.is_just(showing_of(s.phase)) then return n + 1 end
    return n
  end, 0, world.states)
end

---After the slot `closed_pos` is freed, shift every later slot down by one.
---@param states StatesMap
---@param closed_pos integer
---@return StatesMap
local function reflow(states, closed_pos)
  return Map.map_values(function(s)
    return Maybe.from_maybe(
      s,
      Maybe.map(
        function(p) return with(s, { phase = showing(p.bufnr, p.winid, p.pos - 1) }) end,
        Maybe.filter(function(p) return p.pos > closed_pos end, showing_of(s.phase))
      )
    )
  end, states)
end

--- Presentation (pure) ----------------------------------------------------------------------------

---Composite `(client_id, token)` key; `Nothing` for an absent or table token.
---@param client_id integer
---@param token lsp.ProgressToken?
---@return Maybe<string>
local function key_of(client_id, token)
  return Maybe.map(
    function(t) return client_id .. ":" .. tostring(t) end,
    Maybe.filter(function(t) return type(t) ~= "table" end, Maybe.of_nil(token))
  )
end

---Next spinner index (wrapping) and its frame.
---@param idx integer
---@return integer, string
local function advance(idx)
  local n = #ICONS.spinner * CONFIG.spinner_frame_repeats
  local next_idx = idx % n + 1
  return next_idx, ICONS.spinner[math.ceil(next_idx / CONFIG.spinner_frame_repeats)]
end

---`[client] [title]:`
---@param client vim.lsp.Client
---@param value WorkDoneProgressValue
---@return string
local function base(client, value)
  return "["
    .. client.name
    .. "]"
    .. Maybe.from_maybe(
      "",
      Maybe.map(function(t) return " " .. t .. ":" end, Maybe.of_nil(value.title))
    )
end

---`✓ [client] [title]: DONE!`
---@param b string
---@return string
local function done(b) return ICONS.done .. " " .. b .. " DONE!" end

---`⣾ [client] [title]: [message] (NN%)`
---@param b string
---@param value WorkDoneProgressValue
---@param frame string
---@return string
local function progress(b, value, frame)
  return frame
    .. " "
    .. b
    .. table.concat(Maybe.cat_maybes({
      Maybe.map(function(m) return " " .. m end, Maybe.of_nil(value.message)),
      Maybe.map(
        function(p) return string.format(" (%3d%%)", p) end,
        Maybe.of_nil(value.percentage)
      ),
    }))
end

---Fold one LSP value into the state: `end` marks done (spinner frozen); otherwise the
---spinner advances. One allocation per call.
---@param s ProgressState
---@param client vim.lsp.Client
---@param value WorkDoneProgressValue
---@return ProgressState
local function message(s, client, value)
  local b = base(client, value)
  if value.kind == "end" then return with(s, { is_done = true, message = Maybe.just(done(b)) }) end
  local idx, frame = advance(s.spinner_idx)
  return with(
    s,
    { is_done = false, spinner_idx = idx, message = Maybe.just(progress(b, value, frame)) }
  )
end

---@param s ProgressState
---@return string
local function message_text(s) return Maybe.from_maybe("", s.message) end

---@param pos integer
---@param lines integer
---@param cmdheight integer
---@param border Maybe<string>
---@return integer
local function row(pos, lines, cmdheight, border)
  local offset = Maybe.from_maybe(1, Maybe.map(F.const(3), border))
  return lines - cmdheight - 1 - pos * offset
end

---Float config for slot `pos`. `width` is clamped to >= 1 (`nvim_open_win` rejects 0).
---@param pos integer
---@param width integer
---@param env Env
---@return vim.api.keyset.win_config
local function config_of(pos, width, env)
  local w = math.max(width, 1)
  local border = env.border()
  return {
    relative = "editor",
    width = w,
    height = 1,
    row = row(pos, env.lines(), env.cmdheight(), border),
    col = env.columns() - w,
    focusable = false,
    style = "minimal",
    noautocmd = true,
    border = Maybe.from_maybe("", border),
    zindex = CONFIG.window_zindex,
  }
end

--- Effects: acquire / show ------------------------------------------------------------------------

---@type fun(tag: PhaseTag, s: ProgressState, env: Env): Either<ErrorEnv, ProgressState>
local allocate_tag = Match.total(PHASE_TAGS, {
  idle = function(_, s, env)
    return Either.map(
      function(bufnr) return with(s, { phase = ready(bufnr) }) end,
      env.create_buf()
    )
  end,
  ready = function(_, s) return Either.right(s) end,
  showing = function(_, s) return Either.right(s) end,
})

---Ensure the stream exists and owns a buffer. Atomic: one allocation, recorded in the
---returned `World` (later failures are `salvage`d, so the buffer is never orphaned).
---@param world World
---@param key string
---@param client_id integer
---@param env Env
---@return Either<ErrorEnv, World>
local function ensure(world, key, client_id, env)
  local s = Maybe.from_maybe(initial(client_id), Maybe.of_nil(world.states[key]))
  return Either.map(
    function(s2) return update_state(world, key, s2) end,
    allocate_tag(s.phase.tag, s, env)
  )
end

---Open a float: a `Showing` (closed externally) keeps its slot; a `Ready` takes the
---next one. Expected failures leave `world` unchanged (nothing half-allocated).
---@param world World
---@param key string
---@param env Env
---@return Either<ErrorEnv, World>
local function open(world, key, env)
  return with_state(world, key, function(s)
    return Maybe.fold(function() return fault("open without a buffer: " .. key) end, function(bufnr)
      local pos = Maybe.from_maybe(stack_size(world) + 1, pos_of(s.phase))
      return proceed(world, function(width)
        return proceed(
          world,
          function(winid)
            return Either.right(
              update_state(world, key, with(s, { phase = showing(bufnr, winid, pos) }))
            )
          end,
          recover(env.open_win(bufnr, false, config_of(pos, width, env)))
        )
      end, recover(env.strdisplaywidth(message_text(s))))
    end, bufnr_of(s.phase))
  end)
end

---Move a live float to its slot (layout or width change).
---@param world World
---@param key string
---@param env Env
---@return Either<ErrorEnv, World>
local function reposition(world, key, env)
  return with_state(world, key, function(s)
    return Maybe.fold(function() return Either.right(world) end, function(p)
      return proceed(
        world,
        function(width)
          return after(world, attempt(env.set_config(p.winid, config_of(p.pos, width, env))))
        end,
        recover(env.strdisplaywidth(message_text(s)))
      )
    end, live(s, env))
  end)
end

---Write the message into the stream's buffer (when both exist).
---@param world World
---@param key string
---@param env Env
---@return Either<ErrorEnv, World>
local function write(world, key, env)
  return with_state(world, key, function(s)
    return Maybe.from_maybe(
      Either.right(world),
      Maybe.map2(
        function(bufnr, msg) return after(world, attempt(env.set_lines(bufnr, { msg }))) end,
        bufnr_of(s.phase),
        s.message
      )
    )
  end)
end

---No live window -> open; live in this tabpage -> reposition; otherwise -> reopen
---(a tabpage query failure counts as "elsewhere").
---@param s ProgressState
---@param env Env
---@return ProgressAction
local function action(s, env)
  return Maybe.fold(function() return "open" end, function(p)
    local here = Either.either(
      F.const(false),
      function(tp) return tp == env.current_tabpage() end,
      env.win_get_tabpage(p.winid)
    )
    return here and "reposition" or "reopen"
  end, live(s, env))
end

---@param world World
---@param key string
---@param env Env
---@return Either<ErrorEnv, World>
local function open_and_write(world, key, env)
  return Either.chain(open(world, key, env), function(w) return write(w, key, env) end)
end

---@type fun(a: ProgressAction, world: World, key: string, env: Env): Either<ErrorEnv, World>
local render_with = Match.total(RENDER_ACTIONS, {
  ---Close the float in the other tabpage, then reopen here in the same slot.
  ---An expected close failure aborts (no second window).
  reopen = function(_, world, key, env)
    return with_state(world, key, function(s)
      return Maybe.fold(function() return Either.right(world) end, function(p)
        return Either.bind(function(closed)
          if not closed then return Either.right(world) end
          return open_and_write(world, key, env)
        end, attempt(env.close_win(p.winid)))
      end, live(s, env))
    end)
  end,
  reposition = function(_, world, key, env)
    return Either.chain(reposition(world, key, env), function(w) return write(w, key, env) end)
  end,
  open = function(_, world, key, env) return open_and_write(world, key, env) end,
})

---@param world World
---@param key string
---@param env Env
---@return Either<ErrorEnv, World>
local function render(world, key, env)
  return with_state(world, key, function(s) return render_with(action(s, env), world, key, env) end)
end

---Schedule the stay-then-close: bump `gen` (invalidating any pending settle) and post a
---`settle` for this generation.
---@param world World
---@param key string
---@param env Env
---@return Either<ErrorEnv, World>
local function arm(world, key, env)
  return with_state(world, key, function(s)
    local gen = s.gen + 1
    env.post_later(F.immutable({ kind = "settle", key = key, gen = gen }), CONFIG.done_delay_ms)
    return Either.right(update_state(world, key, with(s, { gen = gen })))
  end)
end

--- Effects: release -------------------------------------------------------------------------------

---Close the live window, then delete the buffer. Raw errors: the caller applies policy.
---Idempotent: a retry after a partial success skips what is already gone.
---@param s ProgressState
---@param env Env
---@return Either<ErrorEnv, nil>
local function dispose(s, env)
  local close = Maybe.from_maybe(
    Either.right(nil),
    Maybe.map(function(p) return env.close_win(p.winid) end, live(s, env))
  )
  return Either.bind(
    function()
      return Maybe.from_maybe(
        Either.right(nil),
        Maybe.map(env.delete_buf, Maybe.filter(env.buf_is_valid, bufnr_of(s.phase)))
      )
    end,
    close
  )
end

---After `dispose`: free the slot (reflow + reposition later floats) and reset the state
---to `idle`, keeping `gen` monotonic.
---@param world World
---@param key string
---@param env Env
---@return Either<ErrorEnv, World>
local function retire(world, key, env)
  return with_state(world, key, function(s)
    local reset = update_state(world, key, with(initial(s.client_id), { gen = s.gen }))
    return Maybe.fold(function() return Either.right(reset) end, function(p)
      local w1 = world_of(reflow(reset.states, p.pos))
      return Either.fold(
        function(w, k) return reposition(w, k, env) end,
        w1,
        Seq.filter(function(k)
          return k ~= key
            and Maybe.is_just_and(function(q) return q >= p.pos end, pos_of(w1.states[k].phase))
        end, Map.keys(w1.states))
      )
    end, showing_of(s.phase))
  end)
end

--- Transitions ------------------------------------------------------------------------------------
--- (one per Msg kind)

---ensure -> fold message -> render (salvaged) -> arm if done.
---@param world World
---@param msg MsgProgress
---@param env Env
---@return Either<ErrorEnv, World>
local function on_progress(world, msg, env)
  local data = msg.data
  return Maybe.fold(function() return Either.right(world) end, function(client)
    local key =
      Maybe.from_maybe(tostring(data.client_id), key_of(data.client_id, data.params.token))
    local value = data.params.value --[[@as WorkDoneProgressValue]]
    return Either.chain(ensure(world, key, data.client_id, env), function(w)
      return with_state(
        w,
        key,
        function(s) return Either.right(update_state(w, key, message(s, client, value))) end
      )
    end, function(w) return salvage(w, env, render(w, key, env)) end, function(w)
      if w.states[key].is_done then return arm(w, key, env) end
      return Either.right(w)
    end)
  end, env.get_client_by_id(data.client_id))
end

---Close a finished stream, unless it was re-armed (stale `gen`) or resumed.
---@param world World
---@param msg MsgSettle
---@param env Env
---@return Either<ErrorEnv, World>
local function settle(world, msg, env)
  return when_state(world, msg.key, function(s)
    if s.gen ~= msg.gen or not s.is_done then return Either.right(world) end
    return retrying(
      world,
      msg,
      env,
      Either.chain(dispose(s, env), function() return retire(world, msg.key, env) end)
    )
  end)
end

---Release a stream and forget it.
---@param world World
---@param msg MsgPurge
---@param env Env
---@return Either<ErrorEnv, World>
local function purge(world, msg, env)
  return when_state(world, msg.key, function(s)
    return retrying(
      world,
      msg,
      env,
      Either.chain(
        dispose(s, env),
        function() return retire(world, msg.key, env) end,
        function(w) return Either.right(remove_state(w, msg.key)) end
      )
    )
  end)
end

---Purge a client once it is gone or has no attached buffers.
---@param world World
---@param msg MsgDetach
---@param env Env
---@return Either<ErrorEnv, World>
local function on_detach(world, msg, env)
  local detached = Maybe.is_nothing_or(
    function(c) return next(c.attached_buffers) == nil end,
    env.get_client_by_id(msg.client_id)
  )
  if not detached then return Either.right(world) end
  return Either.fold(
    function(w, k) return purge(w, F.immutable({ kind = "purge", key = k }), env) end,
    world,
    Seq.filter(
      function(k) return world.states[k].client_id == msg.client_id end,
      Map.keys(world.states)
    )
  )
end

---Reposition every live float.
---@param world World
---@param env Env
---@return Either<ErrorEnv, World>
local function on_resize(world, env)
  return Either.fold(
    function(w, k) return reposition(w, k, env) end,
    world,
    Seq.filter(
      function(k) return Maybe.is_just(live(world.states[k], env)) end,
      Map.keys(world.states)
    )
  )
end

---Re-render every slotted stream in the entered tabpage.
---@param world World
---@param env Env
---@return Either<ErrorEnv, World>
local function on_tab_enter(world, env)
  return Either.fold(
    function(w, k) return render(w, k, env) end,
    world,
    Seq.filter(
      function(k) return Maybe.is_just(showing_of(world.states[k].phase)) end,
      Map.keys(world.states)
    )
  )
end

---@type fun(kind: MsgKind, world: World, msg: MSG, env: Env): Either<ErrorEnv, World>
local update_by_kind = Match.total(MSG_KINDS, {
  progress = function(_, w, m, env) return on_progress(w, m, env) end,
  detach = function(_, w, m, env) return on_detach(w, m, env) end,
  resize = function(_, w, _, env) return on_resize(w, env) end,
  tab_enter = function(_, w, _, env) return on_tab_enter(w, env) end,
  settle = function(_, w, m, env) return settle(w, m, env) end,
  purge = function(_, w, m, env) return purge(w, m, env) end,
})

---The transition function. Total over `Msg`; testable with a fake `Env`.
---@param world World
---@param msg MSG
---@param env Env
---@return Either<ErrorEnv, World>
local function update(world, msg, env) return update_by_kind(msg.kind, world, msg, env) end

--- Edge -------------------------------------------------------------------------------------------
--- The one mutable cell, the Env, and the exports

---@type World
local W = world_of({})

-- `ENV` and `dispatch` are mutually recursive (ENV.post -> dispatch -> update(.., ENV)).
-- Lua has no `letrec`, so `ENV` is declared first and bound exactly once below.
---@type Env
local ENV

---W_{n+1} = update(W_n, msg_n). A throw anywhere in `update` becomes a `Left`; a `Left`
---is reported and the previous snapshot is kept. Only ever invoked from the event loop
---(via `post`/`post_later`), hence serial by construction.
---@param msg MSG
local function dispatch(msg)
  W = Either.either(function(err)
    ENV.report(err)
    return W
  end, F.id, Either.join(Either.try_with(classify, update, W, msg, ENV)))
end

ENV = F.immutable({
  open_win = Either.capture_with(classify, vim.api.nvim_open_win),
  close_win = Either.capture_with(
    classify,
    function(winid) return vim.api.nvim_win_close(winid, true) end
  ),
  set_config = Either.capture_with(classify, vim.api.nvim_win_set_config),
  set_lines = Either.capture_with(
    classify,
    function(bufnr, lines) return vim.api.nvim_buf_set_lines(bufnr, 0, 1, false, lines) end
  ),
  create_buf = Either.capture_with(
    classify,
    function() return vim.api.nvim_create_buf(false, true) end
  ),
  delete_buf = Either.capture_with(
    classify,
    function(bufnr) return vim.api.nvim_buf_delete(bufnr, { force = true }) end
  ),
  win_get_tabpage = Either.capture_with(classify, vim.api.nvim_win_get_tabpage),
  strdisplaywidth = Either.capture_with(classify, vim.fn.strdisplaywidth),
  get_client_by_id = function(id) return Maybe.of_nil(vim.lsp.get_client_by_id(id)) end,
  win_is_valid = vim.api.nvim_win_is_valid,
  buf_is_valid = vim.api.nvim_buf_is_valid,
  current_tabpage = vim.api.nvim_get_current_tabpage,
  lines = function() return vim.o.lines end,
  cmdheight = function() return vim.o.cmdheight end,
  columns = function() return vim.o.columns end,
  border = function()
    return Maybe.of_pred(function(s) return s ~= "" end, vim.o.winborder)
  end,
  post = function(msg)
    vim.schedule(function() dispatch(msg) end)
  end,
  post_later = function(msg, ms)
    vim.defer_fn(function() dispatch(msg) end, ms)
  end,
  report = function(err) notify("error", err.message) end,
})

---@class GRIM.progress
---@field on_progress fun(data: vim.event.lspprogress.data)
---@field on_lsp_detach fun(data: vim.event.lspdetach.data)
---@field on_resized fun()
---@field on_tab_enter fun()
---@field _test { update: (fun(world: World, msg: MSG, env: Env): Either<ErrorEnv, World>), world_of: (fun(states: StatesMap): World), initial: (fun(client_id: integer): ProgressState) }
return {
  ---@param data vim.event.lspprogress.data
  on_progress = function(data) ENV.post(F.immutable({ kind = "progress", data = data })) end,
  ---Posting defers past the event, so `attached_buffers` is already updated.
  ---@param data vim.event.lspdetach.data
  on_lsp_detach = function(data)
    ENV.post(F.immutable({ kind = "detach", client_id = data.client_id }))
  end,
  on_resized = function() ENV.post(MSG_RESIZE) end,
  on_tab_enter = function() ENV.post(MSG_TAB_ENTER) end,
  _test = { update = update, world_of = world_of, initial = initial },
}
