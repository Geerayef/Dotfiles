-- ~  Keymaps

vim.g.mapleader = " "

local keymap = vim.keymap.set
keymap("x", "<leader>P", '"_dP', { noremap = true })
keymap("n", "<leader>y", '"+y', { noremap = true })
keymap("n", "<leader>Y", '"+Y', { noremap = true })
keymap("v", "<leader>y", '"+y', { noremap = true })

-- ---------------------------------------- --

-- ~  Options

local o = vim.opt
local cmd = vim.cmd

--  File
cmd.syntax("on")
cmd.filetype("plugin", "indent", "on")
vim.cmd({ cmd = "set", args = { "filetype=man" } })
o.fileformats = "unix"
o.undofile = false

--  Edit
o.virtualedit = "block"

--  UI
o.background = "dark"
o.guicursor = ""
o.signcolumn = "no"
o.cursorline = true
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

--  Behaviour
o.ttyfast = true
o.lazyredraw = true
o.updatetime = 300
o.belloff = "all"
o.scrolloff = 2

--  Search
o.grepprg = "rg --vimgrep --no-heading --smart-case"
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true

--  Color
vim.cmd.colorscheme("habamax")
