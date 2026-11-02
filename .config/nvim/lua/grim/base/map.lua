--- ~ Map operations ------------------------------------------------------------------------- ~ ---
---
---@class GRIM.base.Map
local Map = {}

local F = require("grim.base.func")
local Seq = require("grim.base.seq")

-- Primitives --------------------------------------------------------------------------------------

---Catamorphism over entries via `next`. Iteration order is UNSPECIFIED: `f` must
---be commutative/associative in `acc` unless it halts order-independently.
---`f` must not mutate `t`. Mirrors `Seq.fold_while` (value first, key last).
---@generic K, V, B
---@param f fun(acc: B, v: V, k: K): B, boolean | nil
---@param z B
---@param t table<K, V>
---@return B
function Map.fold_while(f, z, t)
  local function done(_, _, halt) return halt end
  local function step(k, acc)
    local k2, v = next(t, k)
    if k2 == nil then return nil, acc, true end
    local acc2, halt = f(acc, v, k2)
    return k2, acc2, halt or false
  end
  local _, acc = F.until_(done, step, nil, z, false)
  return acc
end

---Total order for deterministic key sorting: by type name, then by value.
---Tables/functions/userdata are unorderable and raise.
---@param a any
---@param b any
---@return boolean
local function key_lt(a, b)
  local ta, tb = type(a), type(b)
  if ta ~= tb then return ta < tb end
  if ta == "boolean" then return (not a) and b end
  if ta == "number" or ta == "string" then return a < b end
  error("map: unorderable key of type " .. ta, 3)
end

-- Folds -------------------------------------------------------------------------------------------

---Key-aware fold. Haskell: `foldlWithKey`. Unordered.
---@see GRIM.base.Map.fold_while
---@generic K, V, B
---@param f fun(acc: B, v: V, k: K): B
---@param z B
---@param t table<K, V>
---@return B
function Map.fold_with_key(f, z, t)
  return Map.fold_while(function(out, v, k) return f(out, v, k), false end, z, t)
end

-- Derived -----------------------------------------------------------------------------------------

---@generic K, V
---@param t table<K, V>
---@return K[] # Sorted: deterministic!
function Map.keys(t)
  local ks = Map.fold_while(function(out, _, k)
    out[#out + 1] = k
    return out
  end, {}, t)
  return Seq.sort_with(key_lt, ks)
end

---Values ordered by sorted key.
---@generic K, V
---@param t table<K, V>
---@return V[]
function Map.values(t)
  return Seq.map(function(k) return t[k] end, Map.keys(t))
end

---`{ key, value }` pairs ordered by sorted key.
---@generic K, V
---@param t table<K, V>
---@return [K, V][]
function Map.entries(t)
  return Seq.map(function(k) return { k, t[k] } end, Map.keys(t))
end

---Inverse of `entries`; later duplicates win.
---@generic K, V
---@param es [K, V][]
---@return table<K, V>
function Map.from_entries(es)
  return Seq.foldl(function(out, e)
    out[e[1]] = e[2]
    return out
  end, {}, es)
end

---`f` must not return nil (that would drop the key).
---@generic K, A, B
---@param f fun(a: A): B
---@param t table<K, A>
---@return table<K, B>
function Map.map_values(f, t)
  return Map.fold_while(function(out, v, k)
    out[k] = f(v)
    return out
  end, {}, t)
end

---@generic K, V
---@param p fun(v: V): boolean
---@param t table<K, V>
---@return table<K, V>
function Map.filter_values(p, t)
  return Map.fold_while(function(out, v, k)
    if p(v) then out[k] = v end
    return out
  end, {}, t)
end

---@generic K, V
---@param out table<K, V>  # mutated: caller owns it
---@param v V
---@param k K
---@return table<K, V>
local function assign(out, v, k)
  out[k] = v
  return out
end

---Shallow, right-biased: keys of `b` override `a`.
---@generic K1, V1, K2, V2
---@param a table<K1, V1>
---@param b table<K2, V2>
---@return table<K1|K2, V1|V2>
function Map.merge(a, b) return Map.fold_while(assign, Map.fold_while(assign, {}, a), b) end

---Right-biased merge of many tables (linear, unlike repeated `merge`).
---@generic K, V
---@param ts table<K, V>[]
---@return table<K, V>
function Map.merge_all(ts)
  return Seq.foldl(function(out, t) return Map.fold_while(assign, out, t) end, {}, ts)
end

---Keep only keys in `ks` (absent keys ignored).
---@generic K, V
---@param ks K[]
---@param t table<K, V>
---@return table<K, V>
function Map.pick(ks, t)
  return Seq.foldl(function(out, k)
    local v = t[k]
    if v ~= nil then out[k] = v end
    return out
  end, {}, ks)
end

---Drop keys in `ks`.
---@generic K, V
---@param ks K[]
---@param t table<K, V>
---@return table<K, V>
function Map.omit(ks, t)
  local dropped = Seq.foldl(function(s, k)
    s[k] = true
    return s
  end, {}, ks)
  return Map.fold_while(function(out, v, k)
    if not dropped[k] then out[k] = v end
    return out
  end, {}, t)
end

---@type GRIM.base.Map
return Map
