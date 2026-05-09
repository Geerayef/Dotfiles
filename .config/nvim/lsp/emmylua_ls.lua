--- @type vim.lsp.Config
return {
  name = "EmmyLua",
  cmd = { "emmylua_ls" },
  filetypes = { "lua" },
  root_markers = GRIM.static.root_markers.lua,
  settings = { emmylua = { workspace = { library = vim.api.nvim_get_runtime_file("", true) } } },
}
