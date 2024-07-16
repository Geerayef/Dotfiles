vim.g.theme = "material"
require("util.static")
require("core.opts")
require("core.func")
require("core.keys")
require("core.diag")
require("lazyTim")
require("core.auto")
if vim.g.neovide then require("core.vide") end
vim.g.material_style = "deep ocean"
vim.cmd.colorscheme("material")
