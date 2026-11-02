--- ~ Either<L, R> --------------------------------------------------------------------------- ~ ---
---
---# Either<L, R> = Left<L> | Right<R>
---@class GRIM.base.monad.Either
local Either = {}

---@class Left<L>
---@field pright false
---@field value L

---@class Right<R>
---@field pright true
---@field value R

---A disjunction: `Left<L>` = failure/cancel, `Right<R>` = success.
---@alias Either<L, R> Left<L> | Right<R>

local F = require("grim.base.func")
local Maybe = require("grim.base.monad.maybe")
local Seq = require("grim.base.seq")

-- Constructors ------------------------------------------------------------------------------------

---Construct a `Left` value.
---@generic L
---@param l L
---@return Left<L>
function Either.left(l) return F.immutable({ pright = false, value = l }) end

---Construct a `Right` value.
---@generic R
---@param r R
---@return Right<R>
function Either.right(r) return F.immutable({ pright = true, value = r }) end

---Lift a `Maybe` into an `Either`, mapping `Nothing` to `Left<L>`.
---@generic L, A
---@param l L
---@param m Maybe<A>
---@return Either<L, A>
function Either.of_maybe(l, m)
  return Maybe.fold(function() return Either.left(l) end, Either.right, m)
end

---`nil` => `Left(l)`, else `Right(a)`.
---@generic L, A
---@param l L
---@param a A | nil
---@return Either<L, A>
function Either.of_nil(l, a)
  if a == nil then return Either.left(l) end
  return Either.right(a)
end

---`Right(a)` iff `pred(a)`, else `Left(l)`.
---@generic L, A
---@param pred fun(a: A): boolean
---@param l L
---@param a A
---@return Either<L, A>
function Either.of_pred(pred, l, a)
  if pred(a) then return Either.right(a) end
  return Either.left(l)
end

---Impure edge: protected-call site. Run `f(...)`; on error: `handler(err)`
---becomes `Left`. Only `f`'s first return value is kept.
---@generic L, A
---@param handler fun(err: any): L
---@param f fun(...: any): A
---@param ... any
---@return Either<L, A>
function Either.try_with(handler, f, ...)
  local n, args = select("#", ...), { ... }
  local ok, res = xpcall(function() return f(unpack(args, 1, n)) end, handler)
  if ok then return Either.right(res) end
  return Either.left(res)
end

---`try_with` with `tostring` as handler.
---@generic A
---@param f fun(...: any): A
---@param ... any
---@return Either<string, A>
function Either.try(f, ...) return Either.try_with(tostring, f, ...) end

---Lift a throwing function into one returning `Either`.
---@generic A
---@param f fun(...: any): A
---@return fun(...: any): Either<string, A>
function Either.capture(f)
  return function(...) return Either.try(f, ...) end
end

---@generic A, L
---@param handler fun(err: any): L
---@param f fun(...: any): A
---@return fun(...: any): Either<L, A>
function Either.capture_with(handler, f)
  return function(...) return Either.try_with(handler, f, ...) end
end

-- Predicates --------------------------------------------------------------------------------------

---Whether the value is a `Left`.
---@generic L, R
---@param e Either<L, R>
---@return boolean
function Either.is_left(e) return not e.pright end

---Whether the value is a `Right`.
---@generic L, R
---@param e Either<L, R>
---@return boolean
function Either.is_right(e) return e.pright end

---`Left` **and** `pred` holds.
---@generic L, R
---@param pred fun(l: L): boolean
---@param e Either<L, R>
---@return boolean
function Either.is_left_and(pred, e)
  if Either.is_left(e) then return pred(e.value) == true end
  return false
end

---`Right` **and** `pred` holds.
---@generic L, R
---@param pred fun(r: R): boolean
---@param e Either<L, R>
---@return boolean
function Either.is_right_and(pred, e)
  if Either.is_right(e) then return pred(e.value) == true end
  return false
end

---Structural equality; `eq_l`/`eq_r` default to `==`.
---@generic L, R
---@param eq_l (fun(a: L, b: L): boolean) | nil
---@param eq_r (fun(a: R, b: R): boolean) | nil
---@param ea Either<L, R>
---@param eb Either<L, R>
---@return boolean
function Either.equal(eq_l, eq_r, ea, eb)
  if ea.pright ~= eb.pright then return false end
  local eq = (Either.is_left(ea)) and eq_l or eq_r
  if eq then return eq(ea.value, eb.value) == true end
  return ea.value == eb.value
end

-- Eliminators -------------------------------------------------------------------------------------

---Case analysis.
---@generic L, R, C
---@param f fun(l: L): C
---@param g fun(r: R): C
---@param e Either<L, R>
---@return C
function Either.either(f, g, e)
  if Either.is_left(e) then return f(e.value) end
  return g(e.value)
end

---Lower from `Right`.
---@generic L, R
---@param default R
---@param e Either<L, R>
---@return R
function Either.from_right(default, e)
  if Either.is_right(e) then return e.value end
  return default
end

---Value, else `f(l)`.
---@generic L, R
---@param f fun(l: L): R
---@param e Either<L, R>
---@return R
function Either.from_right_with(f, e)
  if Either.is_right(e) then return e.value end
  return f(e.value)
end

---Lower from `Left`.
---@generic L, R
---@param default L
---@param e Either<L, R>
---@return L
function Either.from_left(default, e)
  if Either.is_left(e) then return e.value end
  return default
end

---Right -> Just (nil payload -> Nothing); Left -> Nothing.
---@generic L, R
---@param e Either<L, R>
---@return Maybe<R>
function Either.to_maybe(e)
  if Either.is_right(e) then return Maybe.of_nil(e.value) end
  return Maybe.nothing()
end

---Left -> Just (nil payload -> Nothing); Right -> Nothing.
---@generic L, R
---@param e Either<L, R>
---@return Maybe<L>
function Either.to_maybe_left(e)
  if Either.is_left(e) then return Maybe.of_nil(e.value) end
  return Maybe.nothing()
end

---`Right(v)` => { v }.
---@generic L, R
---@param e Either<L, R>
---@return R[]
function Either.to_list(e)
  if Either.is_right(e) then return F.immutable({ e.value }) end
  return F.immutable({})
end

---Value, else throw `msg` (level 2).
---@generic L, R
---@param msg string
---@param e Either<L, R>
---@return R
function Either.expect(msg, e)
  if Either.is_right(e) then return e.value end
  error(msg, 2)
end

-- Functor/Bifunctor -------------------------------------------------------------------------------

---Map over the `Right` branch; a `Left` is unchanged (inert under map).
---@nodiscard
---@generic L, A, B
---@param f fun(a: A): B
---@param e Either<L, A>
---@return Either<L, B>
function Either.map(f, e)
  if Either.is_left(e) then return e end
  return Either.right(f(e.value))
end

---Map the Left branch. Rust: `map_err`. Haskell: `first`.
---@nodiscard
---@generic L, M, R
---@param f fun(l: L): M
---@param e Either<L, R>
---@return Either<M, R>
function Either.map_left(f, e)
  if Either.is_left(e) then return Either.left(f(e.value)) end
  return e
end

---@nodiscard
---@generic L, M, A, B
---@param f fun(l: L): M
---@param g fun(a: A): B
---@param e Either<L, A>
---@return Either<M, B>
function Either.bimap(f, g, e)
  if Either.is_left(e) then return Either.left(f(e.value)) end
  return Either.right(g(e.value))
end

---Swap: `Left(l)` => `Right(l)`; `Right(r)` => `Left(r)`.
---@nodiscard
---@generic L, R
---@param e Either<L, R>
---@return Either<R, L>
function Either.swap(e)
  if Either.is_left(e) then return Either.right(e.value) end
  return Either.left(e.value)
end

---Lift a plain function to Either.
---@generic L, A, B
---@param f fun(a: A): B
---@return fun(e: Either<L, A>): Either<L, B>
function Either.lift(f)
  return function(e) return Either.map(f, e) end
end

-- Applicative/Monad -------------------------------------------------------------------------------

---First Left wins.
---@nodiscard
---@generic L, A, B, C
---@param f fun(a: A, b: B): C
---@param ea Either<L, A>
---@param eb Either<L, B>
---@return Either<L, C>
function Either.map2(f, ea, eb)
  if Either.is_left(ea) then return ea end
  if Either.is_left(eb) then return eb end
  return Either.right(f(ea.value, eb.value))
end

---Applicative apply. Haskell: `<*>`.
---@nodiscard
---@generic L, A, B
---@param ef Either<L, fun(a: A): B>
---@param ea Either<L, A>
---@return Either<L, B>
function Either.ap(ef, ea)
  if Either.is_left(ef) then return ef end
  if Either.is_left(ea) then return ea end
  return Either.right(ef.value(ea.value))
end

---Sequence, discarding the first result. Haskell: `>>`.
---@nodiscard
---@generic L, A, B
---@param ea Either<L, A>
---@param eb Either<L, B>
---@return Either<L, B>
function Either.then_(ea, eb)
  if Either.is_left(ea) then return ea end
  return eb
end

---Pair two `Either`s.
---@nodiscard
---@generic L, A, B
---@param ea Either<L, A>
---@param eb Either<L, B>
---@return Either<L, [A, B]>
function Either.zip(ea, eb)
  return Either.map2(function(a, b) return F.immutable({ a, b }) end, ea, eb)
end

---Bind: a `Left` short-circuits; a `Right` applies `f` and returns
---its `Either`, flattening one level. The monadic composition law holds.
---@generic L, A, B
---@param f fun(a: A): Either<L, B> # `f` produces the next `Either` in the chain.
---@param e Either<L, A>
---@return Either<L, B>
function Either.bind(f, e)
  if Either.is_left(e) then return e end
  return f(e.value)
end

---Flatten one level.
---@generic L, R
---@param e Either<L, Either<L, R>>
---@return Either<L, R>
function Either.join(e)
  if Either.is_left(e) then return e end
  return e.value
end

---Run `f` on the Right value for its effect; return `e`.
---@generic L, R
---@param f fun(r: R): any
---@param e Either<L, R>
---@return Either<L, R>
function Either.tap(f, e)
  if Either.is_right(e) then f(e.value) end
  return e
end

---@generic L, R
---@param f fun(l: L): any
---@param e Either<L, R>
---@return Either<L, R>
function Either.tap_left(f, e)
  if Either.is_left(e) then f(e.value) end
  return e
end

---Either.fold: Monadic fold; halts at the first Left.
---@generic L, R, A
---@param f fun(out: R[], a: A): Either<L, R>
---@param z A[]
---@param xs R[]
function Either.fold(f, z, xs)
  return Seq.fold_while(function(acc, a)
    local e = f(acc.value, a)
    return e, Either.is_left(e)
  end, Either.right(z), xs)
end

-- Alternative -------------------------------------------------------------------------------------

---Recover from a `Left` by applying `f` to the error value (returns an `Either`). A `Right` is kept.
---@generic L, A
---@param f fun(l: L): Either<L, A> # `f` re-attempts, returning a new Either.
---@param e Either<L, A>
---@return Either<L, A>
function Either.or_else(f, e)
  if Either.is_left(e) then return f(e.value) end
  return e
end

---First Right wins; otherwise `eb` (so its Left). Haskell: `<|>` (no Monoid on L).
---@nodiscard
---@generic L, M, A
---@param ea Either<L, A>
---@param eb Either<M, A>
---@return Either<M, A>
function Either.alt(ea, eb)
  if Either.is_right(ea) then return ea end
  return eb
end

---Right failing `pred` becomes Left(l). Haskell: `ensure`.
---@nodiscard
---@generic L, R
---@param pred fun(r: R): boolean
---@param l L
---@param e Either<L, R>
---@return Either<L, R>
function Either.ensure(pred, l, e)
  if Either.is_right(e) and not pred(e.value) then return Either.left(l) end
  return e
end

-- Effects -----------------------------------------------------------------------------------------

---@generic L, R
---@param f fun(r: R): any
---@param e Either<L, R>
---@return nil
function Either.iter(f, e)
  if Either.is_right(e) then f(e.value) end
end

---@generic L, R
---@param f fun(l: L): any
---@param e Either<L, R>
---@return nil
function Either.iter_left(f, e)
  if Either.is_left(e) then f(e.value) end
end

-- Collections -------------------------------------------------------------------------------------

---@param right boolean
---@param es Either<any, any>[]
---@param i integer
---@param n integer
---@param out any[]
---@param j integer # Next write index in `out`.
---@return any[]
local function _pick_from(right, es, i, n, out, j)
  if i > n then return out end
  local e = es[i]
  if right and Either.is_right(e) then
    out[j] = e.value
    j = j + 1
  end
  return _pick_from(right, es, i + 1, n, out, j)
end

---@nodiscard
---@generic L, R
---@param es Either<L, R>[]
---@return L[]
function Either.lefts(es) return _pick_from(false, es, 1, #es, {}, 1) end

---@nodiscard
---@generic L, R
---@param es Either<L, R>[]
---@return R[]
function Either.rights(es) return _pick_from(true, es, 1, #es, {}, 1) end

---`partition(es)` == `lefts(es), rights(es)` in one pass.
---@nodiscard
---@generic L, R
---@param es Either<L, R>[]
---@return L[], R[]
function Either.partition(es)
  local r = Seq.foldl(function(acc, e)
    local side = acc[Either.is_left(e) and 1 or 2]
    side[#side + 1] = e.value
    return acc
  end, { {}, {} }, es)
  return r[1], r[2]
end

---Map with an `Either`-returning `f`; short-circuits on the first `Left`.
---@nodiscard
---@generic L, A, B
---@param f fun(a: A): Either<L, B>
---@param xs A[]
---@return Either<L, B[]>
function Either.traverse(f, xs)
  local r = Seq.fold_while(function(out, x, i)
    local e = f(x)
    if Either.is_left(e) then return e, true end
    out[i] = e.value
    return out
  end, {}, xs)
  if Either.is_left(r) then return r end -- `out` has only integer keys: no `tag`
  return Either.right(r)
end

---All `Right` -> `Right` of values; first `Left` otherwise.
---@nodiscard
---@generic L, R
---@param es Either<L, R>[]
---@return Either<L, R[]>
function Either.sequence(es) return Either.traverse(F.id, es) end

---Left-to-right Kleisli pipeline: `chain(e, f, g)` == `e >>= f >>= g`.
---@generic L, A, B, C, D, E
---@overload fun(e: Either<L, A>, f: fun(a: A): Either<L, B>): Either<L, B>
---@overload fun(e: Either<L, A>, f: (fun(a: A): Either<L, B>), g: fun(b: B): Either<L, C>): Either<L, C>
---@overload fun(e: Either<L, A>, f: (fun(a: A): Either<L, B>), g: (fun(b: B): Either<L, C>), h: fun(c: C): Either<L, D>): Either<L, D>
---@overload fun(e: Either<L, A>, f: (fun(a: A): Either<L, B>), g: (fun(b: B): Either<L, C>), h: (fun(c: C): Either<L, D>), i: fun(d: D): Either<L, E>): Either<L, E>
---@param e Either<L, any>
---@param ... fun(x: any): Either<L, any>
---@return Either<L, any>
function Either.chain(e, ...)
  return Seq.foldl(function(acc, f) return Either.bind(f, acc) end, e, { ... })
end

---@type GRIM.base.monad.Either
return F.immutable(Either)
