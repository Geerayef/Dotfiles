vim.g.theme = "material"
require("util.static")
require("core.opts")
require("core.func")
require("core.keys")
require("core.diag")
require("lazyTim")
require("core.auto")
vim.cmd.colorscheme(vim.g.theme)
if vim.g.neovide then require("core.vide") end
