---@type vim.lsp.Config
return {
  name = "Svelte LS",
  cmd = { "svelteserver", "--stdio" },
  filetypes = { "svelte" },
  root_markers = { "svelte.config.js" },
  settings = {
    typescript = {
      inlayHints = {
        enumMemberValues = { enabled = true },
        functionLikeReturnTypes = { enabled = true },
        parameterNames = { enabled = "literals", suppressWhenArgumentMatchesName = true },
        parameterTypes = { enabled = true },
        propertyDeclarationTypes = { enabled = true },
        variableTypes = { enabled = true },
      },
    },
  },
}
