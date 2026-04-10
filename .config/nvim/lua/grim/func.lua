---@param msg string # Message to display
---@param lvl string # Log level
local notify = function(lvl, msg)
  if lvl == nil or msg == nil then return end
  local level = string.upper(lvl)
  vim.notify("[" .. level .. "] -- " .. msg, vim.log.levels[level])
end

---@class func
---@field notify fun(msg: string, lvl: string): nil
return { notify = notify }
