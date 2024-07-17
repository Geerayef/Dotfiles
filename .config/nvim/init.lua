vim.g.theme = "github_dark_colorblind"
require("util.static")
require("core.opts")
require("core.func")
require("core.keys")
require("core.diag")
require("lazyTim")
require("core.auto")
if vim.g.neovide then require("core.vide") end
vim.cmd.colorscheme(vim.g.theme)
