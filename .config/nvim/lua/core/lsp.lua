vim.lsp.config("*", {
  root_markers = S.root_markers,
  capabilities = {
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
  },
})
