vim.loader.enable()
require("util.static")
require("core.opts")
require("core.func")
require("core.keys")
require("core.auto")
require("core.diag")
if vim.g.neovide then require("core.vide") end
require("lazyTim")
vim.cmd.colorscheme("kanagawa")
vim.cmd.highlight({
  args = { "link", "NormalFloat", "Normal" },
  bang = true,
})
