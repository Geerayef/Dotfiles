vim.lsp.config("luals", {
  name = "LuaLS",
  on_attach = require("lua.util.func").Attach,
  filetypes = { "lua" },
  root_dir = require("util.fs").root(
    vim.api.nvim_buf_get_name(0),
    S.root_markers
  ),
  settings = { Lua = {} },
  cmd = { "lua-language-server" },
})
