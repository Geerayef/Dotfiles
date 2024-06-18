F = {}

function F.LspAttach(client, bufnr)
  Key.LSP(client, bufnr)
  vim.api.nvim_buf_create_user_command(
    bufnr,
    "FormatLSP",
    function(_) vim.lsp.buf.format() end,
    { desc = "Format current buffer with LSP." }
  )
  if client.server_capabilities and client.server_capabilities.code_lens then
    local codelens =
      vim.api.nvim_create_augroup("LSPCodeLens", { clear = true })
    vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave", "CursorHold" }, {
      group = codelens,
      buffer = bufnr,
      callback = function() vim.lsp.codelens.refresh() end,
    })
  end
end

---Map key sequence to action.
---@param mode string|table # Mode{s}
---@param l string # Left side of mapping
---@param r string|function # Right side of mapping
---@param bo table # Buffer options
---@param desc string # Mapping description
function F.map(mode, l, r, bo, desc)
  bo = bo or { noremap = true, silent = true, desc = "" }
  bo.desc = desc or ("[" .. r .. "]")
  vim.keymap.set(mode, l, r, bo)
end

---Get currently active Vim mode.
---@param full boolean # Show full mode name
---@return string
function F.VimMode(full)
  local modes
  if full then
    modes = S.VimModeLowercaseFull
  else
    modes = S.VimModeTwo
  end
  return modes[vim.api.nvim_get_mode().mode] or "[unknown]"
end

---@param buf number # Buffer ID
---@return boolean
function F.IsLargeFile(buf)
  local size_threshold = 1024 * 1024
  local ok, stat = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
  return ok and stat ~= nil and stat.size > size_threshold
end

---@return boolean
function F.IsBufEmpty() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end

---@param buf number # Buffer ID
---@return boolean
function F.IsBufInRepo(buf)
  local buf_path = vim.api.nvim_buf_get_name(buf)
  local gitdir = vim.fn.finddir(".git", buf_path .. ";")
  return gitdir and #gitdir > 0 and #gitdir < #buf_path
end

---@param message string
---@param level string # Log level
function F.Notify(level, message)
  if level == nil or message == nil then return end
  local lvl = level:upper()
  vim.notify("~~~~~ [" .. lvl .. "] " .. message, vim.log.levels[lvl])
end

return F
