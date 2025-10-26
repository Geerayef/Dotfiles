vim.g.theme = "yugen"
require("util.static")
require("core.opts")
require("util.func")
require("core.keys")
require("core.diag")
require("lazyTim")
require("core.lsp")
require("util.lsp")
require("core.auto")
if vim.g.neovide then require("core.vide") end
GRIM.init()
vim.cmd.colorscheme(vim.g.theme)
