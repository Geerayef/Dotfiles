--- ~ Match ---------------------------------------------------------------------------------- ~ ---
---
---@class GRIM.base.Match
local Match = {}

local Maybe = require("grim.base.monad").maybe
local Seq = require("grim.base.seq")

---# Match case
---`{ [discriminant: D] = fn(discriminant: D): A }`
---@alias Case<D> table<D, (fun(d: D, ...: any): any)>

---@generic D
---@param tags D[]
---@param cases Case<D>
---@return fun(d: D, ...: any): any
function Match.total(tags, cases)
  Seq.foldl(function(_, t)
    if cases[t] == nil then error("Match.total: missing case for " .. vim.inspect(t), 2) end
    return nil
  end, nil, tags)
  return function(d, ...)
    local case = cases[d]
    if case == nil then error("Match.total: unhandled " .. vim.inspect(d), 2) end
    return case(d, ...)
  end
end

---Pattern match returning a Maybe<A>: no-match = Nothing (composable, no catch-all).
---Each case is a pure function of the discriminant; `match` never raises.
---@generic A, D
---@param d D # The discriminant (a value / tag / field).
---@param cases Case<D>
---@return Maybe<A>
function Match.match(d, cases)
  local case = cases[d]
  if case == nil then return Maybe.nothing() end
  return Maybe.just(case(d))
end

---Closed-union match: raw `A`, total; errors on a missing case (construction) or an
---unhandled discriminant (runtime). The lowering path for a closed discriminated union.
---@generic D
---@param d D # Discriminant.
---@param cases Case<D>
---@param tags D[] # The closed set of discriminants.
---@return ...
function Match.match_exhaustive(d, cases, tags)
  for _, t in ipairs(tags) do
    if cases[t] == nil then error("match_exhaustive: missing case for " .. vim.inspect(t)) end
  end
  local case = cases[d]
  if case == nil then error("match_exhaustive: unhandled discriminant " .. vim.inspect(d)) end
  return case(d)
end

---@type GRIM.base.Match
return Match
