---@type vim.lsp.Config
return {
  name = "Bash LS",
  cmd = { "bash-language-server", "start" },
  filetypes = { "bash", "sh" },
  root_markers = GRIM.static.root_markers.sh,
  settings = { bashIde = { globPattern = "*@(.bash|.inc|.sh|.command)" } },
}
