--- @type vim.lsp.Config
return {
  name = "Lua LS",
  cmd = { "lua-language-server" },
  filetypes = { "lua" },
  root_markers = GRIM.static.root_markers.lua,
}
