--- ~ Chain<A> ------------------------------------------------------------------------------- ~ ---
---
---# Chain<A>
---## Lazy single-pass sequence backed by a generator.
---@class GRIM.base.monad.Chain
local Chain = { _gen = function() end }
Chain.__index = Chain

---A lazy, single-pass sequence of A, backed by a generator.
---Extends `GRIM.monad.Chain` so instances carry all module methods via `__index`.
---@class Chain<A>: GRIM.base.monad.Chain
---@field _gen fun(): A? # The generator: yields the next A, or nil at exhaustion.

local F = require("grim.base.func")

---Construct a Chain from a generator that returns the next value or nil.
---@generic A
---@param gen fun<A>(): A?
---@return Chain<A>
function Chain.new(gen)
  local c = setmetatable({}, Chain)
  c._gen = gen
  return c
end

---Wrap an array or a pairs-iterable table; yields { key, value } records (lazy).
---The generator is stateful: `ctrl` (the "last key" cursor) is updated on each step
---so that successive calls to the generator advance through the table correctly.
---@generic K, V
---@param t table<K, V>
---@return Chain<{ key: K, value: V }>
function Chain.from_table(t)
  local it, st, ctrl = pairs(t)
  return Chain.new(function()
    local k, v = it(st, ctrl)
    if k == nil then return nil end
    ctrl = k -- advance cursor for the next call
    return F.immutable({ key = k, value = v })
  end)
end

---Wrap an arbitrary generator/iterator.
---@generic A
---@param gen fun<A>(): A?
---@return Chain<A>
function Chain.from_iterator(gen) return Chain.new(gen) end

---Map each yielded value; lazy (deferred).
---@generic A, B
---@param f fun<A, B>(a: A): B
---@return Chain<B>
function Chain:map(f)
  return Chain.new(function()
    local v = self._gen()
    if v == nil then return nil end
    return f(v)
  end)
end

---Keep only values satisfying `p`; lazy.
---@generic A
---@param p fun<A>(a: A): boolean
---@return Chain<A>
function Chain:filter(p)
  return Chain.new(function()
    local v
    while true do
      v = self._gen()
      if v == nil then return nil end
      if p(v) then return v end
    end
  end)
end

---Fold over the sequence (left-associative).
---@generic T, A
---@param f fun<T, A>(acc: T, a: A): T
---@param acc T
---@return T
function Chain:reduce(f, acc)
  local v
  while true do
    v = self._gen()
    if v == nil then break end
    acc = f(acc, v)
  end
  return acc
end

---Run a side effect for each value (lazy drain).
---@generic A
---@param f fun<A>(a: A)
function Chain:for_each(f)
  local v
  while true do
    v = self._gen()
    if v == nil then break end
    f(v)
  end
end

---Materialise to an array (single pass).
---@generic A
---@return A[]
function Chain:to_table()
  local t = {}
  local v
  while true do
    v = self._gen()
    if v == nil then break end
    t[#t + 1] = v
  end
  return F.immutable(t)
end

---@class GRIM.base.monad.Chain
return F.immutable(Chain)
