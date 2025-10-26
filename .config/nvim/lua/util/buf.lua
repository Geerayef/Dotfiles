---@return boolean
-- BUF.empty_p = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end
local empty_p = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end

BUF = {
  empty_p = empty_p,
}
return BUF
