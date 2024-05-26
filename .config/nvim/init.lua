local Tim = require("lazyTim")
require("core.functions")
require("core.options")
require("core.keymaps")
require("lazy").setup("plugins", Tim)
require("core.autocmds")
if vim.g.neovide then require("core.neovide") end
F.DisableBuiltin()
vim.cmd.colorscheme("kanagawa")
