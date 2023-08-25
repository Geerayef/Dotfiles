-- #  ~ Statusline
-- fns = require("myConfig.core.functions")
-- vim.cmd([[
-- function! NvimMode()
--     return luaeval('fns.NvimMode()')
-- endfunction
-- ]])
-- local statusline_str = "%-8.{NvimMode()} - %-3.m - %-4.r - %-32.{FugitiveStatusline()} %= %-t %= - %{get(b:,'gitsigns_status','')} - %4.l:%-3.c (%3.p%%)"

M.disable_builtin()

local opt = vim.opt

opt.shell = "/usr/bin/zsh"

-- ~  File
vim.cmd("syntax enable")
vim.cmd("filetype plugin indent on")
opt.fileformats = "unix"
opt.encoding = "utf-8"
opt.fileencoding = "utf-8"
opt.undofile = true
opt.formatoptions:append("j")
opt.nrformats:remove("octal")

-- ~  Edit
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
opt.virtualedit = "block"

-- ~  Search
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

-- ~  UI
opt.guicursor = ""
opt.cursorline = true
opt.cursorlineopt = "number"
opt.number = true
opt.relativenumber = true
opt.laststatus = 3
opt.showtabline = 0
-- opt.statusline = statusline_str
opt.title = false
opt.showcmd = false
opt.showmode = false
opt.cmdheight = 1
opt.switchbuf = "useopen,uselast"
opt.signcolumn = "yes"
opt.termguicolors = true
opt.ruler = false
opt.wildmenu = true
opt.pumblend = 17
opt.wildmode = "longest:full"
opt.wildoptions = "pum"

-- ~  Behaviour
opt.ttyfast = true
opt.lazyredraw = true
opt.updatetime = 300
opt.belloff = "all"
opt.splitright = true
opt.splitbelow = true
opt.scrolloff = 4
opt.sidescrolloff = 4
opt.mouse = "a"

