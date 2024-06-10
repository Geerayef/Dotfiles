vim.loader.enable()
require("util.static")
require("core.functions")
require("core.options")
require("core.keymaps")
require("core.autocmds")
if vim.g.neovide then require("core.neovide") end
require("lazyTim")
vim.cmd.colorscheme("kanagawa")
