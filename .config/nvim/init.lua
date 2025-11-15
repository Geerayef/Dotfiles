GRIM = {}
vim.g.theme = "yugen"
require("util")
require("core.opts")
require("core.keys")
require("core.diag")
require("core.lsp")
require("core.auto")
require("lazyTim")
if vim.g.neovide then require("core.vide") end
GRIM.tab.init()
vim.cmd.colorscheme(vim.g.theme)
