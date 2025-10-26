vim.lsp.config("*", {
  root_markers = S.root_markers,
  single_file_support = true,
  capabilities = vim.tbl_deep_extend(
    "force",
    vim.lsp.protocol.make_client_capabilities(),
    {
      workspace = { didChangeWatchedFiles = { dynamicRegistration = true } },
      textDocument = {
        documentFormattingProvider = false,
        codelens = { enable = true },
        completion = {
          completionItem = {
            snippetSupport = true,
            resolveSupport = {
              properties = { "detail", "documentation", "additionalTextEdits" },
            },
          },
        },
      },
    }
  ),
})
