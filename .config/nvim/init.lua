vim.g.did_install_default_menus = 1
vim.g.theme = "yugen"
require("grim") --[[@as GRIM]]
require("core.opts")
require("core.keys")
require("core.diag")
require("core.lsp")
require("core.auto")
require("lazyTim")
vim.cmd.colorscheme(vim.g.theme)
