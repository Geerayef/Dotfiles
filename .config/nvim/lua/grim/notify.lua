---@class GRIM.notify
local notify = {}

---@param lvl string  # Log level (e.g. "error"/"warn"/"info"/"debug").
---@param msg string  # Message to display.
---@return nil
function notify.notify(lvl, msg)
  if lvl == nil or msg == nil then return end
  local level = string.upper(lvl)
  vim.notify("[" .. level .. "] -- " .. msg, vim.log.levels[level])
end

---@type GRIM.notify
return notify
