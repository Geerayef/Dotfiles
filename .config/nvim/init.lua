vim.g.theme = "rose-pine"
require("util.static")
require("core.opts")
require("core.func")
require("core.keys")
require("core.diag")
require("lazyTim")
require("core.auto")
require("util.lsp")
vim.cmd.colorscheme(vim.g.theme)
if vim.g.neovide then require("core.vide") end
