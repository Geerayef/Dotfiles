-- ~  Keymaps

vim.g.mapleader = " "
local keymap = vim.keymap.set
keymap("n", "q", "<cmd>q!<CR>", { noremap = true })
keymap("v", "q", "<cmd>q!<CR>", { noremap = true })

-- ---------------------------------------- --

-- ~  Options

local o = vim.opt
local cmd = vim.cmd

--  File
cmd.syntax("on")
cmd.filetype("indent", "on")
o.fileformats = "unix"

--  Edit
o.virtualedit = "block"
o.expandtab = true

--  UI
o.background = "dark"
o.guicursor = ""
o.signcolumn = "no"
o.cursorline = true
o.cursorlineopt = "line"
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
o.number = false

--  Behaviour
o.ttyfast = true
o.lazyredraw = true
o.updatetime = 300
o.belloff = "all"
o.splitright = true
o.splitbelow = true
o.scrolloff = 1
o.sidescrolloff = 1

--  Search
o.grepprg = "rg"
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true

--  Color
vim.cmd("colorscheme default")
vim.cmd("highlight Comment gui=none")
vim.cmd("highlight Normal guibg=#080C10")
vim.cmd("highlight SignColumn guibg=#080C10")
