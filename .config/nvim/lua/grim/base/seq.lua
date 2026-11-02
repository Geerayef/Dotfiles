--- ~ Sequence operations -------------------------------------------------------------------- ~ ---
---
---@class GRIM.base.Seq
local Seq = {}

local F = require("grim.base.func")

-- Primitives --------------------------------------------------------------------------------------

---THE catamorphism. `f` returns `acc, halt`; a truthy `halt` stops early.
---@generic A, B
---@param f fun(acc: B, a: A, i: integer): B, boolean | nil
---@param z B
---@param xs A[]
---@return B
function Seq.fold_while(f, z, xs)
  local n = #xs
  local function done(i, _, halt) return halt or i > n end
  local function step(i, acc)
    local acc2, halt = f(acc, xs[i], i)
    return i + 1, acc2, halt
  end
  local _, acc = F.until_(done, step, 1, z, false)
  return acc
end

---THE anamorphism. `next_(seed)` returns `value, next_seed`; `nil` value stops.
---(Strict: a non-terminating `next_` never returns. Lazy streams come later.)
---@generic S, A
---@param next_ fun(seed: S): A | nil, S | nil
---@param seed S
---@return A[]
function Seq.unfold(next_, seed)
  local out = {}
  local function done(_, _, stop) return stop end
  local function step(s, j)
    local v, s2 = next_(s)
    if v == nil then return s, j, true end
    out[j] = v
    return s2, j + 1, false
  end
  F.until_(done, step, seed, 1, false)
  return out
end

---@generic A
---@param out A[] # Mutated: caller owns it!
---@param ys A[]
---@return A[]
local function append_into(out, ys)
  local m = #out
  return Seq.fold_while(function(acc, y, j)
    acc[m + j] = y
    return acc
  end, out, ys)
end

-- Folds -------------------------------------------------------------------------------------------

---Index-aware left fold. Haskell: `foldlWithIndex`.
---@generic A, B
---@param f fun(acc: B, a: A, i: integer): B
---@param z B
---@param xs A[]
---@return B
function Seq.ifoldl(f, z, xs)
  return Seq.fold_while(function(acc, a, i) return f(acc, a, i), false end, z, xs)
end

---Haskell: `foldl f z xs`.
---@generic A, B
---@param f fun(acc: B, a: A): B
---@param z B
---@param xs A[]
---@return B
function Seq.foldl(f, z, xs)
  return Seq.fold_while(function(acc, a) return f(acc, a), false end, z, xs)
end

---Haskell: `foldr f z xs` (strict).
---@generic A, B
---@param f fun(a: A, acc: B): B
---@param z B
---@param xs A[]
---@return B
function Seq.foldr(f, z, xs) return Seq.foldl(F.flip(f), z, Seq.reverse(xs)) end

-- Derived -----------------------------------------------------------------------------------------

---@generic A
---@param xs A[]
---@return A[]
function Seq.reverse(xs)
  return Seq.unfold(function(i)
    if i < 1 then return nil end
    return xs[i], i - 1
  end, #xs)
end

---`f` must not return nil.
---@generic A, B
---@param f fun(a: A): B
---@param xs A[]
---@return B[]
function Seq.map(f, xs)
  return Seq.fold_while(function(out, a, i)
    out[i] = f(a)
    return out
  end, {}, xs)
end

---List-monad bind (Haskell `concatMap`/`>>=`): the nondeterminism primitive.
---@generic A, B
---@param f fun(a: A): B[]
---@param xs A[]
---@return B[]
function Seq.flat_map(f, xs)
  return Seq.fold_while(function(out, a) return append_into(out, f(a)) end, {}, xs)
end

---@generic A
---@param p fun(a: A): boolean
---@param xs A[]
---@return A[]
function Seq.filter(p, xs)
  return Seq.fold_while(function(out, a)
    if p(a) then out[#out + 1] = a end
    return out
  end, {}, xs)
end

---@generic A
---@param n integer
---@param xs A[]
---@return A[]
function Seq.take(n, xs)
  if n < 1 then return {} end
  return Seq.fold_while(function(out, a, i)
    out[i] = a
    return out, i >= n
  end, {}, xs)
end

---@generic A
---@param n integer
---@param xs A[]
---@return A[]
function Seq.drop(n, xs)
  local k = math.max(n, 0)
  return Seq.fold_while(function(out, a, i)
    if i > k then out[i - k] = a end
    return out
  end, {}, xs)
end

---@generic A
---@param xs A[]
---@param ys A[]
---@return A[]
function Seq.concat(xs, ys) return append_into(append_into({}, xs), ys) end

---Stops at the shorter sequence.
---@generic A, B, C
---@param f fun(a: A, b: B): C
---@param xs A[]
---@param ys B[]
---@return C[]
function Seq.zip_with(f, xs, ys)
  local m = #ys
  return Seq.fold_while(function(out, a, i)
    if i > m then return out, true end
    out[i] = f(a, ys[i])
    return out
  end, {}, xs)
end

---@generic A, B
---@param xs A[]
---@param ys B[]
---@return [A, B][]
function Seq.zip(xs, ys)
  return Seq.zip_with(function(a, b) return { a, b } end, xs, ys)
end

---@generic A
---@param p fun(a: A): boolean
---@param xs A[]
---@return integer | nil
function Seq.find_index(p, xs)
  return Seq.fold_while(function(_, a, i)
    if p(a) then return i, true end
    return nil
  end, nil, xs)
end

---@generic A
---@param p fun(a: A): boolean
---@param xs A[]
---@return boolean
function Seq.any(p, xs) return Seq.find_index(p, xs) ~= nil end

---@generic A
---@param p fun(a: A): boolean
---@param xs A[]
---@return boolean
function Seq.all(p, xs) return not Seq.any(F.negate(p), xs) end

-- Sorting (stable merge sort; input untouched) ----------------------------------------------------

---@generic A
---@param lt fun(a: A, b: A): boolean
---@param a A[]
---@param b A[]
---@return A[]
local function merge(lt, a, b)
  local na, nb, out = #a, #b, {}
  local function done(i, j) return i > na and j > nb end
  local function step(i, j, k)
    -- take from `b` only if strictly less: ties go left, hence stability
    if j > nb or (i <= na and not lt(b[j], a[i])) then
      out[k] = a[i]
      return i + 1, j, k + 1
    end
    out[k] = b[j]
    return i, j + 1, k + 1
  end
  F.until_(done, step, 1, 1, 1)
  return out
end

---Non-tail, but depth <= log2(n).
---@generic A
---@param lt fun(a: A, b: A): boolean
---@param xs A[]
---@param lo integer
---@param hi integer
---@return A[]
local function msort(lt, xs, lo, hi)
  if lo > hi then return {} end
  if lo == hi then return { xs[lo] } end
  local mid = math.floor((lo + hi) / 2)
  return merge(lt, msort(lt, xs, lo, mid), msort(lt, xs, mid + 1, hi))
end

---Stable sort by strict-less-than `lt`.
---@generic A
---@param lt fun(a: A, b: A): boolean
---@param xs A[]
---@return A[]
function Seq.sort_with(lt, xs) return msort(lt, xs, 1, #xs) end

---Stable sort by key (computed once per element); keys compared with `<`,
---so they must be mutually comparable (all numbers or all strings).
---@generic A, K : number|string
---@param key fun(a: A): K
---@param xs A[]
---@return A[]
function Seq.sort_by(key, xs)
  local decorated = Seq.map(function(x) return { key(x), x } end, xs)
  local sorted = Seq.sort_with(function(p, q) return p[1] < q[1] end, decorated)
  return Seq.map(function(p) return p[2] end, sorted)
end

---@type GRIM.base.Seq
return Seq
