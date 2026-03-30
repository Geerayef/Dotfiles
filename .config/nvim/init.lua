vim.g.did_install_default_menus = 1
vim.g.theme = "yugen"
GRIM = {}
require("grim.static")
require("grim.func")
require("grim.fs")
require("grim.git")
require("grim.lsp")
require("grim.pql")
require("core.opts")
require("core.keys")
require("core.diag")
require("core.lsp")
require("core.auto")
require("lazyTim")
if vim.g.neovide then require("core.vide") end
GRIM.tab.init()
vim.cmd.colorscheme(vim.g.theme)
