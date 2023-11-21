-- ~  Keymaps

vim.g.mapleader = " "
local keymap = vim.keymap.set
keymap("n", "q", "<cmd>q!<CR>")

-- ---------------------------------------- --

-- ~  Options

local o = vim.opt
local cmd = vim.cmd

o.shell = "/usr/bin/fish"

--  File
cmd.syntax("on")
cmd.filetype("plugin", "indent", "on")
o.fileformats = "unix"

--  Edit
o.virtualedit = "block"
o.expandtab = true

--  UI
o.background = "dark"
o.guicursor = ""
o.signcolumn = "no"
o.cursorline = true
o.cursorlineopt = "number"
o.termguicolors = true
o.laststatus = 0
o.showtabline = 0
o.title = false
o.showcmd = false
o.showmode = false
o.showmatch = true
o.cmdheight = 1
o.ruler = false
o.wildmenu = true

--  Behaviour
o.ttyfast = true
o.lazyredraw = true
o.updatetime = 300
o.belloff = "all"
o.splitright = true
o.splitbelow = true
o.scrolloff = 10
o.sidescrolloff = 10

--  Search
o.grepprg = "rg"
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true

--  Color
vim.cmd("colorscheme slate")
vim.cmd("highlight Comment gui=none")
vim.cmd("highlight Normal guibg=#000000")
vim.cmd("highlight SignColumn guibg=#000000")
