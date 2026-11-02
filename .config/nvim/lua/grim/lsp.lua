---@class GRIM.lsp
local lsp = {}

---LSP on_attach function.
---@function GRIM.lsp.attach
---@param client vim.lsp.Client
---@param bufid integer
---@return nil
function lsp.attach(client, bufid)
  Key.LSP(client, bufid)
  if client and client.name ~= "markdown_oxide" then
    vim.lsp.codelens.enable(true, { bufnr = bufid })
  end
end

---GRIM.lsp provides LSP related utilities.
---@type GRIM.lsp
return lsp
