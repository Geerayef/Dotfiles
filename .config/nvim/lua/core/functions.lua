F = {}

-- ~  --------------------------------------------------------------------------------  ~ --

function F.LspAttach(client, bufnr)
  Key.LSP(client, bufnr)
  vim.api.nvim_buf_create_user_command(
    bufnr,
    "FormatLSP",
    function(_) vim.lsp.buf.format() end,
    { desc = "Format current buffer with LSP." }
  )
  if client.server_capabilities and client.server_capabilities.code_lens then
    local codelens = vim.api.nvim_create_augroup("LSPCodeLens", { clear = true })
    vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave", "CursorHold" }, {
      group = codelens,
      callback = function() vim.lsp.codelens.refresh() end,
      buffer = bufnr,
    })
  end
end

-- ~  --------------------------------------------------------------------------------  ~ --

---Map key sequence to action.
---@param mode string|table Mode{s}
---@param l string Left side of mapping
---@param r string|function Right side of mapping
---@param bo table Buffer options
---@param desc string Mapping description
function F.map(mode, l, r, bo, desc)
  bo.desc = desc
  vim.keymap.set(mode, l, r, bo)
end

-- ~  --------------------------------------------------------------------------------  ~ --

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

-- ~ -------------------------------------------------------------------------------- ~ --

---@param group string
---@vararg { [1]: string|string[], [2]: vim.api.keyset.create_autocmd }
---@return nil
function F.mk_autocmd(group, ...)
  local id = vim.api.nvim_create_augroup(group, {})
  for _, a in ipairs({ ... }) do
    a[2].group = id
    vim.api.nvim_create_autocmd(unpack(a))
  end
end

-- ~ -------------------------------------------------------------------------------- ~ --

function F.DisableBuiltinProviders()
  local default_providers = { "node", "perl", "ruby" }
  for _, provider in ipairs(default_providers) do
    vim.g[("loaded_" .. provider .. "_provider")] = 0
  end
end

-- ~  --------------------------------------------------------------------------------  ~ --

---@param bufnr number
---@return boolean
function F.IsBigBuff(bufnr)
  local large_file_threshold = 1024 * 1024
  local ok, stat = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(bufnr))
  return ok and stat ~= nil and stat.size > large_file_threshold
end

-- ~  --------------------------------------------------------------------------------  ~ --

---@return boolean
function F.IsBufEmpty() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end

-- ~  --------------------------------------------------------------------------------  ~ --

---@return boolean
function F.IsGitRepo()
  local bufferpath = vim.fn.expand("%:p:h")
  local gitdir = vim.fn.finddir(".git", bufferpath .. ";")
  return gitdir and #gitdir > 0 and #gitdir < #bufferpath
end

-- ~  --------------------------------------------------------------------------------  ~ --

---@param devicons table # Object `nvim-web-devicons`
---@return table<string, string> # Filetype icon & icon color
function F.GetFtIcon(devicons)
  local full_filename = vim.api.nvim_buf_get_name(0)
  local filename = vim.fn.fnamemodify(full_filename, ":t")
  local extension = vim.fn.fnamemodify(filename, ":e")
  local ftype_icon, ftype_icon_color = devicons.get_icon_color(filename, extension, { default = true })
  return ftype_icon and { ftype_icon .. "", ftype_icon_color }
end

-- ~  --------------------------------------------------------------------------------  ~ --

return F
