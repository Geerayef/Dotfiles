GRIM = {}
vim.g.theme = "yugen"
require("util.static")
require("core.opts")
require("util.func")
require("core.keys")
require("core.diag")
require("core.lsp")
require("util.lsp")
require("lazyTim")
require("core.auto")
if vim.g.neovide then require("core.vide") end
GRIM.tab.init()
vim.cmd.colorscheme(vim.g.theme)
