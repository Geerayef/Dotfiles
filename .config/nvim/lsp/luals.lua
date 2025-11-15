vim.lsp.config("luals", {
  name = "Lua LS",
  cmd = { "lua-language-server" },
  on_attach = require("util.lsp").attach,
  filetypes = { "lua" },
  root_markers = {
    ".luarc.json",
    ".luarc.jsonc",
    "*.lua",
    "init.lua",
    ".luacheckrc",
    ".stylua.toml",
    "lazy-lock.json",
  },
  settings = { Lua = {} },
  root_dir = require("util.fs").root(
    vim.api.nvim_buf_get_name(0),
    S.root_markers
  ),
})
