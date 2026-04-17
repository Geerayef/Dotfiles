vim.lsp.config("*", {
  root_markers = vim.iter(vim.tbl_values(GRIM.static.root_markers)):flatten():totable(),
  single_file_support = true,
  on_attach = GRIM.lsp.attach,
  root_dir = GRIM.fs.root(vim.api.nvim_buf_get_name(0)),
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
    },
    (function()
      local ok, blink = pcall(require, "blink.cmp")
      if ok and blink.get_lsp_capabilities then
        return blink.get_lsp_capabilities(vim.lsp.config["*"].capabilities)
      end
    end)()
  ),
} --[[@as vim.lsp.Config]])
