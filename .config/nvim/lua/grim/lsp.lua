---LSP on_attach function.
---@function GRIM.lsp.attach
---@param client vim.lsp.Client
---@param bufid integer
---@return nil
local attach = function(client, bufid)
  Key.LSP(client, bufid)
  vim.lsp.codelens.enable(true, { bufnr = bufid })
end

---@package GRIM.lsp
---GRIM.lsp provides LSP related utilities.
---@class GRIM.lsp
---@field attach fun(client: vim.lsp.Client, bufid: integer): nil
return { attach = attach }
