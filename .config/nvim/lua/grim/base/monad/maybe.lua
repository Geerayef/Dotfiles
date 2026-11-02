--- ~ Maybe<A> ------------------------------------------------------------------------------- ~ ---
---
---# Maybe<A> = Just<A> | Nothing
---@class GRIM.base.monad.Maybe
local Maybe = {}

---@class Just<A>
---@field pjust true
---@field value A

---@class Nothing
---@field pjust false

---A presence: `Just<A>` = present, `Nothing` = absent.
---Invariant: `value` is `A` iff `pjust == true`; accessing `value` on a Nothing
---(`pjust == false`) returns `nil` at runtime but is a type error — always guard
---with `is_just` / `bind` before lowering. Typed `A` (not `A?`) to eliminate
---nullable noise at every guarded call site (mirrors Haskell `Just a` field type).
---@alias Maybe<A> Just<A> | Nothing

local F = require("grim.base.func")
local Seq = require("grim.base.seq")

---@type Nothing
local NOTHING = F.immutable({ pjust = false })

-- Constructors ------------------------------------------------------------------------------------

---Construct a `Just` value.
---@generic A
---@param a A
---@return Just<A>
function Maybe.just(a)
  assert(a ~= nil, "Maybe.just: payload must not be nil")
  return F.immutable({ pjust = true, value = a })
end

---Construct a `Nothing` value (polymorphic: usable as Maybe of any type).
---@return Nothing
function Maybe.nothing() return NOTHING end

---Lift a possibly-nil value into a `Maybe` (`nil` -> `Nothing`, else `Just`).
---@generic A
---@param a A | nil
---@return Maybe<A>
function Maybe.of_nil(a)
  if a == nil or a == vim.NIL then return Maybe.nothing() end
  return Maybe.just(a)
end

---`Just<A>` iff `pred(a)`.
---@generic A
---@param pred fun(a: A): boolean
---@param a A
---@return Maybe<A>
function Maybe.of_pred(pred, a)
  if pred(a) then return Maybe.just(a) end
  return Maybe.nothing()
end

---First element of a sequence.
---@generic A
---@param xs A[]
---@return Maybe<A>
function Maybe.head(xs) return Maybe.of_nil(xs[1]) end

---First element of `xs` satisfying `pred`.
---@generic A
---@param p fun(a: A): boolean
---@param xs A[]
---@return Maybe<A>
function Maybe.find(p, xs)
  return Maybe.map(function(i) return xs[i] end, Maybe.of_nil(Seq.find_index(p, xs)))
end

-- Predicates --------------------------------------------------------------------------------------

---Whether the value is a `Just`.
---@generic A
---@param m Maybe<A>
---@return boolean
function Maybe.is_just(m) return m.pjust end

---Whether the value is a `Nothing`.
---@generic A
---@param m Maybe<A>
---@return boolean
function Maybe.is_nothing(m) return not m.pjust end

---`Just` **and** `pred` holds.
---@generic A
---@param pred fun(a: A): boolean
---@param m Maybe<A>
---@return boolean
function Maybe.is_just_and(pred, m)
  if Maybe.is_just(m) then return pred(m.value) == true end
  return false
end

---`Nothing`, **or** `pred` holds.
---@generic A
---@param pred fun(a: A): boolean
---@param m Maybe<A>
---@return boolean
function Maybe.is_nothing_or(pred, m)
  if Maybe.is_just(m) then return pred(m.value) == true end
  return true
end

-- Eliminators -------------------------------------------------------------------------------------

---Catamorphism.
---`from_maybe(default, m)` == `maybe(default, identity, m)`.
---@generic A, B
---@param default B
---@param f fun(a: A): B
---@param m Maybe<A>
---@return B
function Maybe.maybe(default, f, m)
  if Maybe.is_just(m) then return f(m.value) end
  return default
end

---Lazy catamorphism: both branches are functions.
---`fold(function() return default end, f, m)` == `maybe(default, f, m)`.
---@generic A, B
---@param none fun(): B
---@param some fun(a: A): B
---@param m Maybe<A>
---@return B
function Maybe.fold(none, some, m)
  if Maybe.is_just(m) then return some(m.value) end
  return none()
end

---Lower to a raw value: the `Just`'s value, else the fallback `default`.
---@generic A
---@param default A
---@param m Maybe<A>
---@return A
function Maybe.from_maybe(default, m)
  if Maybe.is_just(m) then return m.value end
  return default
end

---Fallback to `f()` on `Nothing` (returns a `Just`).
---@generic A
---@param f fun(): Maybe<A>
---@param m Maybe<A>
---@return Maybe<A>
function Maybe.from_maybe_lazy(f, m)
  if Maybe.is_just(m) then return m end
  return Maybe.just(f())
end

---Zero-or-one element list.
---@generic A
---@param m Maybe<A>
---@return A[]
function Maybe.to_list(m)
  if Maybe.is_just(m) then return F.immutable({ m.value }) end
  return F.immutable({})
end

-- Functor/Applicative/Monad -----------------------------------------------------------------------

---Map the `Just` branch; a `Nothing` is unchanged.
---@generic A, B
---@param f fun(a: A): B
---@param m Maybe<A>
---@return Maybe<B>
function Maybe.map(f, m)
  if Maybe.is_nothing(m) then return Maybe.nothing() end
  return Maybe.just(f(m.value))
end

---Bind: a `Nothing` short-circuits; a `Just` applies `f` (which returns a `Maybe`).
---@generic A, B
---@param f fun(a: A): Maybe<B>
---@param m Maybe<A>
---@return Maybe<B>
function Maybe.bind(f, m)
  if Maybe.is_nothing(m) then return Maybe.nothing() end
  return f(m.value)
end

---Lift a plain function to `Maybe`.
---@generic A, B
---@param f fun(a: A): B
---@return fun(m: Maybe<A>): Maybe<B>
function Maybe.lift(f)
  return function(m) return Maybe.map(f, m) end
end

---Lift over two `Maybe`s.
---@generic A, B, C
---@param f fun(a: A, b: B): C
---@param ma Maybe<A>
---@param mb Maybe<B>
---@return Maybe<C>
function Maybe.map2(f, ma, mb)
  if Maybe.is_just(ma) and Maybe.is_just(mb) then return Maybe.just(f(ma.value, mb.value)) end
  return Maybe.nothing()
end

---Lift over three `Maybe`s.
---@generic A, B, C, D
---@param f fun(a: A, b: B, c: C): D
---@param ma Maybe<A>
---@param mb Maybe<B>
---@param mc Maybe<C>
---@return Maybe<D>
function Maybe.map3(f, ma, mb, mc)
  if Maybe.is_just(ma) and Maybe.is_just(mb) and Maybe.is_just(mc) then
    return Maybe.just(f(ma.value, mb.value, mc.value))
  end
  return Maybe.nothing()
end

---Applicative apply.
---@generic A, B
---@param mf Maybe<(fun(a: A): B)>
---@param ma Maybe<A>
---@return Maybe<B>
function Maybe.ap(mf, ma)
  if Maybe.is_just(mf) and Maybe.is_just(ma) then return Maybe.just(mf.value(ma.value)) end
  return Maybe.nothing()
end

---Sequence, discarding the first result.
---@generic A, B
---@param ma Maybe<A>
---@param mb Maybe<B>
---@return Maybe<B>
function Maybe.then_(ma, mb)
  if Maybe.is_just(ma) then return mb end
  return Maybe.nothing()
end

---Flatten one level.
---@generic A
---@param mma Maybe<Maybe<A>>
---@return Maybe<A>
function Maybe.join(mma)
  if Maybe.is_nothing(mma) then return Maybe.nothing() end
  return mma.value
end

---Keep the value only if `pred` holds.
---@generic A
---@param pred fun(a: A): boolean
---@param m Maybe<A>
---@return Maybe<A>
function Maybe.filter(pred, m)
  if Maybe.is_just(m) and pred(m.value) then return m end
  return Maybe.nothing()
end

---Pair two `Maybe`s.
---@generic A, B
---@param ma Maybe<A>
---@param mb Maybe<B>
---@return Maybe<[A, B]>
function Maybe.zip(ma, mb)
  return Maybe.map2(function(a, b) return { a, b } end, ma, mb)
end

---@generic A, B
---@param m Maybe<[A, B]>
---@return Maybe<A>, Maybe<B>
function Maybe.unzip(m)
  if Maybe.is_just(m) then return Maybe.just(m.value[1]), Maybe.just(m.value[2]) end
  return Maybe.nothing(), Maybe.nothing()
end

---Run `f` on the value for its effect, return `m` unchanged.
---@generic A
---@param f fun(a: A): any
---@param m Maybe<A>
---@return Maybe<A>
function Maybe.tap(f, m)
  if Maybe.is_just(m) then f(m.value) end
  return m
end

-- Alternative -------------------------------------------------------------------------------------

---Strict alternative: First `Just` wins.
---@generic A
---@param ma Maybe<A>
---@param mb Maybe<A>
---@return Maybe<A>
function Maybe.alt(ma, mb)
  if Maybe.is_just(ma) then return ma end
  return mb
end

---First `Just` in a sequence.
---@generic A
---@param ms Maybe<A>[]
---@return Maybe<A>
function Maybe.choice(ms)
  return Maybe.bind(function(i) return ms[i] end, Maybe.of_nil(Seq.find_index(Maybe.is_just, ms)))
end

-- Effects -----------------------------------------------------------------------------------------

---Apply `f` to the value if present.
---@generic A
---@param f fun(a: A): any
---@param m Maybe<A>
---@return nil
function Maybe.iter(f, m)
  if Maybe.is_just(m) then f(m.value) end
end

---Apply the (possibly effectful) `f` to the value of every `Just` in
---the batch `ms`, in order; `Nothing` elements are skipped (`f` is never run on them).
---Standard `forEach` combinator over an iterable of `Maybe`s: terminal and effectful —
---`f`'s results are discarded, returns `nil`. No short-circuit: one absent element
---does not suppress the effects for later elements.
---@generic A
---@param f fun(a: A): any
---@param ms Maybe<A>[]
---@return nil
function Maybe.iter_all(f, ms)
  return Seq.foldl(function(_, m) return Maybe.iter(f, m) end, nil, ms)
end

-- Collections -------------------------------------------------------------------------------------

---Map with a `Maybe`-returning `f`; short-circuits on first `Nothing`.
---@generic A, B
---@param f fun(a: A): Maybe<B>
---@param xs A[]
---@return Maybe<B[]>
function Maybe.traverse(f, xs)
  local r = Seq.fold_while(function(out, x, i)
    local m = f(x)
    if Maybe.is_just(m) then
      out[i] = m.value
      return out, false
    end
    return Maybe.nothing(), true
  end, {}, xs)
  if Maybe.is_nothing(r) then return Maybe.nothing() end
  return Maybe.just(F.immutable(r))
end

---All `Just` -> `Just` of values; any `Nothing` -> `Nothing`.
---@generic A
---@param ms Maybe<A>[]
---@return Maybe<A[]>
function Maybe.sequence(ms) return Maybe.traverse(F.id, ms) end

---Map then drop `Nothing`s.
---@generic A, B
---@param f fun(a: A): Maybe<B>
---@param xs A[]
---@return B[]
function Maybe.map_maybes(f, xs) return Seq.flat_map(F.compose(Maybe.to_list, f), xs) end

---Values of all `Just`s.
---@generic A
---@param ms Maybe<A>[]
---@return A[]
function Maybe.cat_maybes(ms) return Seq.flat_map(Maybe.to_list, ms) end

---@type GRIM.base.monad.Maybe
return F.immutable(Maybe)
