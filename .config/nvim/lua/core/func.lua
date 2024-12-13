F = {}

function F.LSPAttach(client, bufnr)
  Key.LSP(client, bufnr)
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

---@param msg string # Message to display
---@param lvl string # Log level
function F.Notify(lvl, msg)
  if lvl == nil or msg == nil then return end
  local ok, notify = pcall(require, "notify")
  if not ok then return end
  local level = lvl:upper()
  notify("[" .. level .. "] " .. msg, lvl)
end

---Map key sequence to action.
---Verbose. Buffer local.
---@param mode string|table # Mode{s}
---@param l string # Left side of mapping
---@param r string|function # Right side of mapping
---@param buf number # Buffer ID
---@param desc string # Mapping description
function F.bmap(mode, l, r, buf, desc)
  local bo = { buffer = buf, desc = "" }
  bo.desc = desc or ("[" .. r .. "]")
  vim.keymap.set(mode, l, r, bo)
end

---Map key sequence to action.
---Wrapper for `vim.keymap.set`.
---@param mode string|table # Mode{s}
---@param l string # Left side of mapping
---@param r string|function # Right side of mapping
---@param desc string # Mapping description
function F.map(mode, l, r, desc)
  local bo = { silent = true, desc = "" }
  bo.desc = desc or (type(r) == "string" and ("[" .. r .. "]"))
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

return F
