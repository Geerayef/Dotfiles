local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
local border = O.Border
vim.opt.rtp:prepend(lazypath)
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
return {
  defaults = { version = "*" },
  dev = { path = "~/dev" },
  checker = { enabled = false },
  change_detection = { notify = false },
  ui = {
    title = "Lazy",
    border = border,
  },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
}
