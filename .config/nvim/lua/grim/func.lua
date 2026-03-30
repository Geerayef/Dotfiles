---@param msg string # Message to display
---@param lvl string # Log level
local notify = function(lvl, msg)
  if lvl == nil or msg == nil then return end
  local level = lvl:upper()
  vim.notify("[" .. level .. "] -- " .. msg, vim.log.levels[lvl])
end

F = { notify = notify }
return F
