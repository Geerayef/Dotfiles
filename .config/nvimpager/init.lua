-- ~  Keymaps

vim.g.mapleader = " "

local keymap = vim.keymap.set
keymap("x", "<leader>P", '"_dP', { noremap = true })
keymap("n", "<leader>y", '"+y' , { noremap = true })
keymap("n", "<leader>Y", '"+Y' , { noremap = true })
keymap("v", "<leader>y", '"+y' , { noremap = true })

-- ---------------------------------------- --

-- ~  Options

local o = vim.opt
local cmd = vim.cmd

--  File
cmd.syntax("on")

--  Edit
o.virtualedit = "block"

--  UI
o.background = "dark"
o.guicursor = ""
o.signcolumn = "no"
o.cursorline = true
-- o.cursorlineopt = "number"
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
o.number = true
o.relativenumber = true

--  Behaviour
o.ttyfast = true
o.lazyredraw = true
o.updatetime = 300
o.belloff = "all"
o.splitright = true
o.splitbelow = true
o.scrolloff = 2
o.sidescrolloff = 1

--  Search
o.grepprg = "rg"
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true

--  Color
-- lunaperche slate elflord
vim.cmd("colorscheme elflord")
-- vim.cmd("highlight Comment gui=none")
-- vim.cmd("highlight Normal guibg=#080C10")
-- vim.cmd("highlight SignColumn guibg=#080C10")
