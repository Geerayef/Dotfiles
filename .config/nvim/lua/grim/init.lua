---@class GRIM
---@field static GRIM.static
---@field fs GRIM.fs
---@field func GRIM.func
---@field git GRIM.git
---@field hl GRIM.hl
---@field lsp GRIM.lsp
---@field line GRIM.line
GRIM = {
  static = require("grim.static"),
  fs = require("grim.fs"),
  git = require("grim.git"),
  lsp = require("grim.lsp"),
}
return setmetatable(GRIM, { __index = function(_, k) return require("grim." .. k) end })
