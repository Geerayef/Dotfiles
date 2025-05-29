vim.g.theme = "yugen"
require("util.static")
require("core.opts")
require("core.func")
require("core.keys")
require("core.lsp")
require("lazyTim")
require("core.diag")
require("core.auto")
if vim.g.neovide then require("core.vide") end
vim.cmd.colorscheme(vim.g.theme)
