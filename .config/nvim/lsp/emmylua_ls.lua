--- @type vim.lsp.Config
return {
  name = "EmmyLua",
  cmd = { "emmylua_ls" },
  filetypes = { "lua" },
  root_markers = GRIM.static.root_markers.lua,
  on_attach = function() Key.LSP(_, vim.api.nvim_get_current_buf()) end,
}
