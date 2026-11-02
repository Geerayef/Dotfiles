--- ~ Functional combinators ----------------------------------------------------------------- ~ ---
---
---@class GRIM.base.Func
local F = {}

local MT = {
  __newindex = function() error("Attempt to modify immutable table") end,
  __metatable = false,
}

--- Utility ----------------------------------------------------------------------------------------

---@param i integer
---@return boolean
local function pzero(i) return i == 0 end

---@param p fun(...: any): any
---@return boolean
local function holds(p, ...) return p(...) and true or false end

---Non-tail by necessity (builds a multi-value result); depth == #fns.
---@param fns (fun(...: any): any)[]
---@param i integer
---@param n integer
---@return any ...
local function fan(fns, i, n, ...)
  if i > n then return end
  return fns[i](...), fan(fns, i + 1, n, ...)
end

---Immutable (copy-based) guard: reads (incl. `pairs` / `#`) work; writing new keys raises.
---Shallow copy — nested tables are shared (only the outer table is guarded).
---@generic T: table
---@param t T
---@return T
function F.immutable(t)
  local copy = {}
  for k, v in pairs(t) do
    copy[k] = v
  end
  return setmetatable(copy, MT)
end

--- Combinators ------------------------------------------------------------------------------------

---@generic A
---@overload fun(done: (fun(a: A): boolean), step: (fun(a: A): A), a: A): A
---@param done fun(...: any): boolean
---@param step fun(...: any): any  # returns the next state (multi-value)
---@param ... any
---@return any ...
local function until_(done, step, ...)
  if done(...) then return ... end
  return until_(done, step, step(...))
end
F.until_ = until_

---Identity.
---@generic A
---@param a A
---@return A
function F.id(a) return a end

---Constant: a function that ignores its argument and returns `c`.
---@generic A
---@param c A
---@return fun(...: any): A
function F.const(c)
  return function() return c end
end

---Swap the first two arguments of `f` (variadic passthrough).
---@generic A, B, C
---@param f fun(a: A, b: B, ...: any): C
---@return fun(b: B, a: A, ...: any): C
function F.flip(f)
  return function(b, a, ...) return f(a, b, ...) end
end

---Curry a two-argument function into two successive one-argument functions.
---@generic A, B, C
---@param f fun(a: A, b: B): C
---@return fun(a: A): fun(b: B): C
function F.curry(f)
  return function(a)
    return function(b) return f(a, b) end
  end
end

---Curry a three-argument function into three successive one-arugment functions.
---@generic A, B, C, D
---@param f fun(a: A, b: B, c: C): D
---@return fun<A>(a: A): fun(b: B): fun(c: C): D
function F.curry3(f)
  return function(a)
    return function(b)
      return function(c) return f(a, b, c) end
    end
  end
end

---Uncurry a two-curried function `f` into a single function accepting two arugments.
---@generic A, B, C
---@param f fun(a: A): fun(b: B): C
---@return fun(a: A, b: B): C
function F.uncurry(f)
  return function(a, b) return f(a)(b) end
end

---Uncurry a three-curried function `f` into a single function accepting two arugments.
---@generic A, B, C, D
---@param f fun(a: A): fun(b: B): fun(c: C): D
---@return fun(a: A, b: B, c: C): D
function F.uncurry3(f)
  return function(a, b, c) return f(a)(b)(c) end
end

---`f $ x`: apply `f` to the arguments.
---@generic A, B, C, R
---@overload fun(f: fun(a: A): R, a: A): R
---@overload fun(f: fun(a: A, b: B): R, a: A, b: B): R
---@overload fun(f: fun(a: A, b: B, c: C): R, a: A, b: B, c: C): R
---@param f fun(...: any): any
---@param ... any
---@return any
function F.apply(f, ...) return f(...) end

---Partially apply leading arguments to `f` (nil-safe; variadic; `any` is unavoidable).
---@generic A, B, C, R
---@overload fun(f: (fun(a: A): R), a: A): fun(): R
---@overload fun(f: (fun(a: A, b: B): R), a: A): fun(b: B): R
---@overload fun(f: (fun(a: A, b: B): R), a: A, b: B): fun(): R
---@overload fun(f: (fun(a: A, b: B, c: C): R), a: A): fun(b: B, c: C): R
---@overload fun(f: (fun(a: A, b: B, c: C): R), a: A, b: B): fun(c: C): R
---@param f fun(...: any): any
---@param ... any
---@return fun(...: any): any
function F.partial(f, ...)
  local pre, n = { ... }, select("#", ...)
  local function step(i, ...) return i - 1, pre[i], ... end
  return function(...) return f(select(2, until_(pzero, step, n, ...))) end
end

---Left-to-right pipeline: apply the given functions in order (heterogeneous chain);
---all return values are threaded onward.
---@param ... fun(...: any): any
---@return fun(...: any): any
function F.pipe(...)
  local fns, n = { ... }, select("#", ...)
  local function done(i) return i > n end
  local function step(i, ...) return i + 1, fns[i](...) end
  return function(...) return select(2, until_(done, step, 1, ...)) end
end

---Right-to-left composition of N functions: `compose(f, g, h)(x) == f(g(h(x)))`.
---@param ... fun(...: any): any
---@return fun(...: any): any
function F.compose(...)
  local fns, n = { ... }, select("#", ...)
  local function step(i, ...) return i - 1, fns[i](...) end
  return function(...) return select(2, until_(pzero, step, n, ...)) end
end

---Fan-out (Haskell `&&&`): one input, every function's result as multiple returns.
---@generic A, B, C, D
---@overload fun(f: (fun(a: A): B), g: fun(a: A): C): fun(a: A): B, C
---@overload fun(f: (fun(a: A): B), g: (fun(a: A): C), h: fun(a: A): D): fun(a: A): B, C, D
---@param ... fun(...: any): any
---@return fun(...: any): any ...
local function juxt(...)
  local fns, n = { ... }, select("#", ...)
  return function(...) return fan(fns, 1, n, ...) end
end
F.juxt = juxt

---Fan-out then fan-in: `converge(h, f, g)(x) == h(f(x), g(x))`.
---@generic A, B, C, D, R
---@overload fun(combine: (fun(b: B, c: C): R), f: (fun(a: A): B), g: fun(a: A): C): fun(a: A): R
---@overload fun(combine: (fun(b: B, c: C, d: D): R), f: (fun(a: A): B), g: (fun(a: A): C), h: fun(a: A): D): fun(a: A): R
---@param combine fun(...: any): any
---@param ... fun(...: any): any
---@return fun(...: any): any
function F.converge(combine, ...)
  local branches = juxt(...)
  return function(...) return combine(branches(...)) end
end

---`stop` is the verdict that short-circuits (false for all_of, true for any_of).
---@param stop boolean
---@param ps (fun(...: any): any)[]
---@param n integer
---@return fun(...: any): boolean
local function quantify(stop, ps, n)
  local function done(i, flag) return flag == stop or i > n end
  local function step(i, _, ...) return i + 1, holds(ps[i], ...), ... end
  return function(...)
    local _, flag = until_(done, step, 1, not stop, ...)
    return flag
  end
end

---Conjunction of predicates (short-circuits; `all_of()` is true).
---@generic A
---@param ... fun(a: A): boolean
---@return fun(a: A): boolean
function F.all_of(...) return quantify(false, { ... }, select("#", ...)) end

---Disjunction of predicates (short-circuits; `any_of()` is false).
---@generic A
---@param ... fun(a: A): boolean
---@return fun(a: A): boolean
function F.any_of(...) return quantify(true, { ... }, select("#", ...)) end

---Wrap `f` in a one-argument function, applying `f` to `a` returning `a`.
---@generic A
---@param f fun(a: A): any
---@return fun(a: A): A
function F.tap(f)
  return function(a)
    f(a)
    return a
  end
end

---@param p fun(...: any): boolean
---@return fun(...: any): boolean
function F.negate(p)
  return function(...) return not p(...) end
end

---`on(op, f)(a, b) == op(f(a), f(b))`.
---@generic A, B, C
---@param op fun(x: B, y: B): C
---@param f fun(a: A): B
---@return fun(a: A, b: A): C
function F.on(op, f)
  return function(a, b) return op(f(a), f(b)) end
end

---Anonymous recursion: `fix(f)(...) == f(fix(f), ...)`. Recursive call in tail
---position stays a proper tail call.
---@param f fun(self: (fun(...: any): any), ...: any): any
---@return fun(...: any): any
function F.fix(f)
  local g
  g = function(...) return f(g, ...) end
  return g
end

---Memoise a one-argument function by argument identity (once per distinct arg).
---@generic A, B
---@param f fun(a: A): B
---@return fun(a: A): B
function F.memoize(f)
  local cache = {}
  return function(x)
    local k = x == nil and {} or x
    local hit = cache[k]
    if hit == nil then
      local r = f(x)
      hit = r == nil and {} or r
      cache[k] = hit
    end
    if hit == {} then return nil end
    return hit
  end
end

---Run `f` at most once; later calls return the first result.
---@generic A
---@param f fun(): A
---@return fun(): A
function F.once(f)
  local done, result = false, nil
  return function()
    if not done then
      result = f()
      done = true
    end
    return result
  end
end

---@type GRIM.base.Func
return F.immutable(F)
