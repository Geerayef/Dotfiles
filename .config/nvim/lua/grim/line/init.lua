---@class GRIM.line
---GRIM.line provides {status,tab} line modules.
---@field status GRIM.line.status
---@field tab GRIM.line.tab
return {
  status = require("grim.line.status"),
  tab = require("grim.line.tab"),
} --[[@as GRIM.line]]
