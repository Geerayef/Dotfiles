---@class GRIM
---@field static GRIM.static
---@field fs GRIM.fs
---@field func GRIM.func
---@field git GRIM.git
---@field hl GRIM.hl?
---@field lsp GRIM.lsp
---@field line GRIM.line?

---@type GRIM
GRIM = {
  ---@package GRIM.static
  static = require("grim.static"),
  ---@package GRIM.fs
  fs = require("grim.fs"),
  ---@package GRIM.git
  git = require("grim.git"),
  ---@package GRIM.lsp
  lsp = require("grim.lsp"),
  ---@package GRIM.func
  func = require("grim.func"),
}
return setmetatable(GRIM, { __index = function(_, k) return require("grim." .. k) end })
