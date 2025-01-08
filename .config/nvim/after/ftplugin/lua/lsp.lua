vim.opt.shiftwidth = 2

local luals = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "lua" },
  root_patterns = {
    ".luarc.json",
    ".luarc.jsonc",
    "*.lua",
    "init.lua",
    ".luacheckrc",
    ".stylua.toml",
    "lazy-lock.json",
  },
  settings = { Lua = {} },
  cmd = { "lua-language-server" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(luals) end
  )
end)
