vim.g.did_install_default_menus = 1
vim.g.background = "dark"
vim.g.theme = "mfd-stealth"
require("grim")
require("core.opts")
require("core.keys")
require("core.diag")
require("core.lsp")
require("core.auto")
require("lazyTim")
vim.cmd.colorscheme(vim.g.theme)
