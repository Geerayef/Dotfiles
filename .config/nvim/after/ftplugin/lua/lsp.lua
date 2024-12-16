vim.opt.shiftwidth = 2

local luals = {
  on_attach = require("core.func").LSPAttach,
  on_init = function(client)
    local path = client.workspace_folders[1].name
    if
      vim.loop.fs_stat(path .. "/.luarc.json")
      or vim.loop.fs_stat(path .. "/.luarc.jsonc")
    then
      return
    end
  end,
  filetypes = { "lua" },
  root_patterns = {
    "*.lua",
    "init.lua",
    ".luacheckrc",
    ".luarc.json",
    ".luarc.jsonc",
    ".stylua.toml",
    "lazy-lock.json",
  },
  settings = { Lua = { runtime = { version = "LuaJIT" } } },
  cmd = { "lua-language-server" },
}

require("util.lsp").start(luals)
