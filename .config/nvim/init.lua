vim.g.did_install_default_menus = 1
GRIM = {}
vim.g.theme = "yugen"
require("grim.static")
require("grim.func")
require("grim.fs")
require("grim.buf")
require("grim.git")
require("grim.hl")
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
