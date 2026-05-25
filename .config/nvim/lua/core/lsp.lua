vim.lsp.config("*", {
  single_file_support = true,
  on_attach = GRIM.lsp.attach,
  root_dir = function(bufid, on) return on(GRIM.fs.root(bufid)) end,
  root_markers = vim.iter(GRIM.static.root_markers):flatten():totable(),
  capabilities = vim.tbl_deep_extend(
    "force",
    vim.lsp.protocol.make_client_capabilities(),
    {
      workspace = { didChangeWatchedFiles = { dynamicRegistration = true } },
      textDocument = {
        documentFormattingProvider = false,
        codelens = { enable = true },
        semanticTokens = { multilineTokenSupport = true },
        completion = {
          completionItem = {
            snippetSupport = true,
            resolveSupport = { properties = { "detail", "documentation", "additionalTextEdits" } },
          },
        },
      },
    } --[[@as lsp.ClientCapabilities]],
    (function()
      local ok, blink = pcall(require, "blink.cmp")
      if ok and blink.get_lsp_capabilities then
        return blink.get_lsp_capabilities(vim.lsp.config["*"].capabilities)
      end
    end)() --[[@as lsp.ClientCapabilities]]
  ) --[[@as lsp.ClientCapabilities]],
} --[[@as vim.lsp.Config]])
