-- #  ~ Statusline
-- fns = require("myConfig.core.functions")
-- vim.cmd([[
-- function! NvimMode()
--     return luaeval('fns.NvimMode()')
-- endfunction
-- ]])
-- local statusline_str = "%-8.{NvimMode()} - %-3.m - %-4.r - %-32.{FugitiveStatusline()} %= %-t %= - %{get(b:,'gitsigns_status','')} - %4.l:%-3.c (%3.p%%)"

local opt = vim.opt

opt.shell = "/usr/bin/zsh"

-- ~  Disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- ~  File settings
vim.cmd("syntax enable")
vim.cmd("filetype plugin indent on")
opt.fileformats = "unix"
opt.encoding = "utf-8"
opt.fileencoding = "utf-8"

-- ~  Line
opt.tabstop = 4
opt.softtabstop = -1
opt.shiftwidth = 4
opt.expandtab = true
opt.smarttab = true
opt.autoindent = true
opt.smartindent = true
opt.breakindent = true
opt.wrap = true
opt.backspace:append("indent,eol,start")
opt.complete:remove("i")
opt.completeopt = "menu,menuone,noinsert,noselect"

-- ~  Search
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

-- ~  Visuals
opt.guicursor = ""
opt.cursorline = true
opt.cursorlineopt = "number"
opt.number = true
opt.relativenumber = true
opt.laststatus = 3
opt.showtabline = 0
-- opt.statusline = statusline_str
opt.showcmd = false
opt.showmode = false
opt.switchbuf = "useopen,uselast"
opt.signcolumn = "yes"
opt.termguicolors = true
opt.ruler = false

-- ~  Miscellaneous
opt.undofile = true
opt.wildmenu = true
-- Delete comment character when joining commented lines
opt.formatoptions:append("j")
opt.nrformats:remove("octal")
opt.mouse = "a"
opt.updatetime = 300
opt.splitright = true
opt.splitbelow = true
opt.scrolloff = 2
opt.sidescrolloff = 5

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
})
