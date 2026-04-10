---@class GRIM
---@field fs fs
---@field func func
---@field git git
---@field hl hl
---@field lsp lsp
---@field static static
---@field tab tab
GRIM = {}
return setmetatable(GRIM, { __index = function(_, k) return require("grim." .. k) end })
