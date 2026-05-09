---@class GRIM
---@field fs GRIM.fs
---@field func GRIM.func
---@field git GRIM.git
---@field hl GRIM.hl
---@field lsp GRIM.lsp
---@field static GRIM.static
GRIM = {
  fs = require("grim.fs"),
  func = require("grim.func"),
  git = require("grim.git"),
  hl = require("grim.hl"),
  lsp = require("grim.lsp"),
  static = require("grim.static"),
}
return setmetatable(GRIM, { __index = function(_, k) return require("grim." .. k) end })
