local border = require("util.objects").Border
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
return {
  defaults = { version = "*" },
  dev = { path = "~/Software" },
  checker = { enabled = false },
  change_detection = { notify = false },
  ui = { title = "Lazy", border = border },
}
