--- LSP on_attach function.
---@function GRIM.lsp.attach
---@param client vim.lsp.Client
---@param bufid integer
---@return nil
local attach = function(client, bufid)
  Key.LSP(client, bufid)
  vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave", "CursorHold" }, {
    group = vim.api.nvim_create_augroup("LSPCodeLens", { clear = true }),
    buffer = bufid,
    callback = function() vim.lsp.codelens.enable(true, { bufnr = bufid }) end,
  })
end

---@class lsp
---@field attach fun(client: vim.lsp.Client, bufid: integer): nil
return {
  attach = attach,
}
