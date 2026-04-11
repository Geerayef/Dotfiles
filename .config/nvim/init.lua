vim.g.did_install_default_menus = 1
vim.g.theme = "yugen"
GRIM = require("grim")
require("core.opts")
require("core.keys")
require("core.diag")
require("core.lsp")
require("core.auto")
require("lazyTim")
require("vim._core.ui2").enable()
vim.cmd.colorscheme(vim.g.theme)
