---@param msg string # Message to display
---@param lvl string # Log level
local notify = function(lvl, msg)
  if lvl == nil or msg == nil then return end
  local level = lvl:upper()
  vim.notify("[" .. level .. "] -- " .. msg, vim.log.levels[lvl])
end

---Get currently active Vim mode.
---@param full boolean # Show full mode name
---@return string
local vimode = function(full)
  local modes
  if full then
    modes = S.VimModeLowercaseFull
  else
    modes = S.VimModeTwo
  end
  return modes[vim.api.nvim_get_mode().mode] or "[unknown]"
end

F = {
  notify = notify,
  vimode = vimode,
}
return F
