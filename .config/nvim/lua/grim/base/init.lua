---@class GRIM.base
---@field func GRIM.base.Func
---@field seq GRIM.base.Seq
---@field map GRIM.base.Map
---@field monad GRIM.base.monad
---@field match GRIM.base.Match
local base = {
  func = require("grim.base.func"),
  seq = require("grim.base.seq"),
  map = require("grim.base.map"),
  monad = require("grim.base.monad"),
  match = require("grim.base.match"),
}
return setmetatable(base, { __index = function(_, k) return require("grim.base." .. k) end })
