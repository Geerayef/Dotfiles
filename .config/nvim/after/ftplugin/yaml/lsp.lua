---@type vim.lsp.Config
local yamlls = {
  on_attach = LSP.attach,
  cmd = { "yaml-language-server", "--stdio" },
  filetypes = { "yaml", "yaml.docker-compose", "yaml.github" },
  root_markers = { "*.yaml", "*.yml" },
  settings = {
    redhat = { telemetry = { enabled = false } },
    yaml = {
      format = { enable = false },
      hover = true,
    },
  },
}

vim.schedule(function()
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(yamlls) end)
end)
