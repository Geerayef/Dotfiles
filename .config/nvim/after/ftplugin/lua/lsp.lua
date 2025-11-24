vim.opt.tabstop = 2
vim.opt.shiftwidth = 2

---@type vim.lsp.Config
local luals = {
  name = "Lua LS",
  cmd = { "lua-language-server" },
  on_attach = LSP.attach,
  filetypes = { "lua" },
  root_markers = { "*.lua", "init.lua" },
  settings = { Lua = {} },
}

---@type vim.lsp.Config
local emmyluals = {
  name = "EmmyLua",
  cmd = { "emmylua_ls" },
  on_attach = LSP.attach,
  filetypes = { "lua" },
  root_markers = { "*.lua", "init.lua" },
}

vim.lsp.config("emmylua_ls", emmyluals)

vim.schedule(function()
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(emmyluals) end)
end)
