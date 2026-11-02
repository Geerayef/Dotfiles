---@class GRIM
---@field static   GRIM.static
---@field base     GRIM.base
---@field fs       GRIM.fs
---@field git      GRIM.git
---@field hl       GRIM.hl
---@field lsp      GRIM.lsp
---@field line     GRIM.line
---@field progress GRIM.progress
---@field notify   GRIM.notify
GRIM = {
  static = require("grim.static"),
  base = require("grim.base"),
  fs = require("grim.fs"),
  git = require("grim.git"),
  lsp = require("grim.lsp"),
}

-- TODO: Define GRIM opts type!
return setmetatable(GRIM, { __index = function(_, k) return require("grim." .. k) end })
