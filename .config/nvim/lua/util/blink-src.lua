local M = {}

function M.new() return setmetatable({}, { __index = M }) end

function M:get_completions(context, callback)
  local ok, completions =
    pcall(vim.fn["conjure#client#completions"], context.cursor_before_line)
  if ok and completions then
    callback({ items = completions, is_incomplete_forward = false })
  else
    callback({ items = {}, is_incomplete_forward = false })
  end
end

return M
