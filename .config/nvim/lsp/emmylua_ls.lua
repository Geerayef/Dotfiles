---@type vim.lsp.Config
return {
  name = "EmmyLua LS",
  cmd = { "emmylua_ls", "--editor", "neovim" },
  filetypes = { "lua" },
  root_markers = GRIM.static.root_markers,
  settings = { codeLens = { enable = true }, hint = { enable = true } },
  workspace_required = false,
}
