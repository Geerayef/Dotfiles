vim.o.tabstop = 2
vim.opt.shiftwidth = 2

local luals = {
  name = "Lua LS",
  cmd = { "lua-language-server" },
  on_attach = LSP.attach,
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
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(luals) end
  )
end)
