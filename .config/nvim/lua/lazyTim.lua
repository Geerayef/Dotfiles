local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
local opts = {
  defaults = { version = "*" },
  dev = { path = "~/Software", patterns = {}, fallback = false },
  checker = { enabled = false },
  change_detection = { notify = false },
  performance = { rtp = { disabled_plugins = { "tutor" } } },
}
require("lazy").setup("plugins", opts)
