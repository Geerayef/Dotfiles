local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
vim.opt.rtp:prepend(lazypath)
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
return {
  defaults = { version = "*" },
  dev = { path = "~/dev" },
  checker = { enabled = false },
  change_detection = { notify = false },
  ui = {
    title = "Lazy",
    border = require("util.objects").Border,
  },
  performance = {
    rtp = {
      disabled_plugins = {
        "2html_plugin",
        "bugreport",
        "compiler",
        "ftplugin",
        "getscript",
        "getscriptPlugin",
        "gzip",
        "logipat",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "optwin",
        "remote_plugins",
        "rrhelper",
        "spellfile_plugin",
        "syntax",
        "synmenu",
        "tarPlugin",
        "tohtml",
        "tutor",
        "tutor_mode_plugin",
        "vimball",
        "vimballPlugin",
        "zipPlugin",
      },
    },
  },
}
