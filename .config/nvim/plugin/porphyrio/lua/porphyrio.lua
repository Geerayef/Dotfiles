--- Porphyrio

-- Types -----------------------------------------------------------------------

---@alias unary_t fun(a: any)
---@alias binary_t fun(a: any, b: any)
---@alias ternary_t fun(a: any, b: any, c: any)
---@alias discard_t any

---@class porphyrio
---@field compose binary_t
---@field compose2 binary_t
---@field compose3 binary_t
---@field cross ternary_t
---@field cross2 ternary_t
---@field distribute binary_t
---@field double unary_t
---@field double2 unary_t
---@field drop binary_t
---@field drop2 binary_t
---@field flip unary_t
---@field flip2 unary_t
---@field fst unary_t
---@field snd unary_t
---@field split binary_t
---@field split2 binary_t
---@field splitr binary_t

-- Combinators -----------------------------------------------------------------

--- Bluebird | `compose`
--- # Unary function, composing two unary functions: λx.f (g x) | f(g(x))
---@type binary_t
---@param f unary_t
---@param g unary_t
---@return unary_t
local bluebird = function(f, g)
  return function(x) return f(g(x)) end
end

--- Blackbird | `compose2`
--- # Binary function, composing a unary and a binary function: λxy.f (g x y) | f(g(x, y))
---@type binary_t
---@param f unary_t
---@param g binary_t
---@return binary_t
local blackbird = function(f, g)
  return function(x, y) return f(g(x, y)) end
end

--- Bunting | `compose3`
--- # Ternary function, composing a unary and a ternary function: λxyz.f (g x y z) | f(g(x, y, z))
---@type binary_t
---@param f unary_t
---@param g ternary_t
---@return ternary_t
local bunting = function(f, g)
  return function(x, y, z) return f(g(x, y, z)) end
end

--- Cardinal | `flip`
--- # Binary function, applying `f` to its arguments in reverse: λxy.f y x | f(y, x)
---@type unary_t
---@param f binary_t
---@return binary_t
local cardinal = function(f)
  return function(x, y) return f(y, x) end
end

--- Cardinal2 | `flip2`
--- # Ternary function, applying `f` to its arguments, reversing the first two: λxyz.f y x z | f(y, x, z)
---@type unary_t
---@param f ternary_t
---@return ternary_t
local cardinal2 = function(f)
  return function(x, y, z) return f(y, x, z) end
end

--- Starling | `split`
--- # Unary function, applying `f` to its argument and result of applying `g` to its argument: λx.f x (g x) | f(x, g(x))
---@type binary_t
---@param f binary_t
---@param g unary_t
---@return unary_t
local starling = function(f, g)
  return function(x) return f(x, g(x)) end
end

--- Starling2 | `split2`
--- # Unary function, applying `f` to its arguments and result of applying `g` to its arguments: λx.f x (g x) | f(x, g(x))
---@type binary_t
---@param f ternary_t
---@param g binary_t
---@return binary_t
local starling2 = function(f, g)
  return function(x, y) return f(x, y, g(x, y)) end
end

--- Violet Starling | `splitr`
--- # Unary function, applying `f` to result of applying `g` to its argument and its argument:
---@type binary_t
---@param f binary_t
---@param g unary_t
---@return unary_t
---λx.f x (g x) | f(x, g(x))
local starling_violet = function(f, g)
  return function(x) return f(g(x), x) end
end

--- Warbler | `double`
--- # Unary function, applying `f` to its argument, doubling it: λx.f x x | f(x, x)
---@type unary_t
---@param f binary_t
---@return unary_t
local warbler = function(f)
  return function(x) return f(x, x) end
end

--- Warbler2 | `double2`
--- # Binary function, applying `f` to its arguments, doubling the last: λxy.f x y y | f(x, y, y)
---@type unary_t
---@param f ternary_t
---@return binary_t
local warbler2 = function(f)
  return function(x, y) return f(x, y, y) end
end

--- Ψ | `distribute`
--- # Binary function, composing a binary and a unary function: λxy.f (g x) (g y) | f(g(x), g(y))
---@type binary_t
---@param f binary_t
---@param g unary_t
---@return binary_t
local psi = function(f, g)
  return function(x, y) return f(g(x), g(y)) end
end

--- Φ (Phoenix) | `cross`
--- # Unary function, composing a binary and two unary functions: λx.f (g x) (h x) | f(g(x), h(x))
---@type ternary_t
---@param f binary_t
---@param g unary_t
---@param h unary_t
---@return unary_t
local phi = function(f, g, h)
  return function(x) return f(g(x), h(x)) end
end

--- Φ1 (Pheasant) | `cross2`
--- # Binary function, composing binary-unary-binary functions: λxy.f (g x) (h x y) | f(g(x), h(x, y))
---@type ternary_t
---@param f binary_t
---@param g binary_t
---@param h binary_t
---@return binary_t
local phi2 = function(f, g, h)
  return function(x, y) return f(g(x, y), h(x, y)) end
end

--- Kestrel | `drop`
--- # First argument to this function, a constant: λxy.x | f(x, y) -> x
---@type binary_t
---@param x any
---@param _ discard_t
---@return any
local kestrel = function(x, _) return x end

--- Kite | `drop2`
--- # Second argument to this function, a constant: λxy.y | f(x, y) -> y
---@type binary_t
---@param _ discard_t
---@param y any
---@return any
local kite = function(_, y) return y end

-- Convient unary/binary operations --------------------------------------------

--- Head | `fst`
--- # First element of `x`
---@type unary_t
---@param x table
---@return any
local fst = function(x) return x[1] end

--- Neck | `snd`
--- # Second element of `x`
---@type unary_t
---@param x table
---@return any
local snd = function(x) return x[2] end

-- Porphyrio: finalise ---------------------------------------------------------

---@type porphyrio
Porphyrio = {
  compose = bluebird,
  compose2 = blackbird,
  compose3 = bunting,
  cross = phi,
  cross2 = phi2,
  distribute = psi,
  double = warbler,
  double2 = warbler2,
  drop = kestrel,
  drop2 = kite,
  flip = cardinal,
  flip2 = cardinal2,
  fst = fst,
  snd = snd,
  split = starling,
  split2 = starling2,
  splitr = starling_violet,
}
return Porphyrio
