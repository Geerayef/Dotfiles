local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
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
require("lazy").setup({
  spec = "plug",
  defaults = { version = "*" },
  rocks = { enabled = false },
  dev = { path = "~/dev" },
  checker = { enabled = false },
  change_detection = { notify = false },
  ui = { title = "Lazy", border = S.Border },
  performance = {
    rtp = {
      disabled_plugins = {
        "bugreport",
        "editorconfig",
        "getscript",
        "getscriptPlugin",
        "gzip",
        "logipat",
        "matchit",
        "matchparen",
        "menu",
        "netrw",
        "netrwPlugin",
        "netrwSettings",
        "netrwFileHandlers",
        "rrhelper",
        "rplugin",
        "tar",
        "tarPlugin",
        "2html_plugin",
        "tohtml",
        "tutor",
        "zip",
        "zipPlugin",
        "vimball",
        "vimballPlugin",
      },
    },
  },
})
