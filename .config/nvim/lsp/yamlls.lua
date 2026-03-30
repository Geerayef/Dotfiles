---@type vim.lsp.Config
return {
  nme = "YAML LS",
  cmd = { "yaml-language-server", "--stdio" },
  filetypes = { "yaml", "yaml.docker-compose", "yaml.gitlab" },
  root_markers = { "*.yaml", "*.yml" },
  settings = {
    redhat = { telemetry = { enabled = false } },
    yaml = { format = { enable = false }, hover = true },
  },
}
