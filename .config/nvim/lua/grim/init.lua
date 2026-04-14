---@class GRIM
---@field fs GRIM.fs
---@field func GRIM.func
---@field git GRIM.git
---@field hl GRIM.hl
---@field lsp GRIM.lsp
---@field static GRIM.static
---@field tab GRIM.tab
GRIM = {}
return setmetatable(GRIM, { __index = function(_, k) return require("grim." .. k) end })
