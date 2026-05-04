---LSP on_attach function.
---@function GRIM.lsp.attach
---@param client vim.lsp.Client
---@param bufid integer
---@return nil
local attach = function(client, bufid)
  Key.LSP(client, bufid)
  vim.lsp.codelens.enable(true, { bufnr = bufid })
  vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave", "CursorHold" }, {
    group = vim.api.nvim_create_augroup("LSPCodeLens", { clear = true }),
    buffer = bufid,
    callback = function() end,
  })
end

---GRIM.lsp provides LSP related utilities.
---@class GRIM.lsp
---@field attach fun(client: vim.lsp.Client, bufid: integer): nil
---@type GRIM.lsp
return { attach = attach }
