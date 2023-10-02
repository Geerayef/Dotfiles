local opt = vim.opt

F.disable_builtin()

opt.shell = "/usr/bin/zsh"

-- ~  File
vim.cmd("syntax enable")
vim.cmd("filetype indent on")
opt.fileformats = "unix"
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

-- ~  UI
opt.background = "dark"
opt.guicursor = ""
opt.cursorline = true
opt.cursorlineopt = "number"
opt.termguicolors = true
opt.number = true
opt.relativenumber = true
opt.laststatus = 3
opt.showtabline = 1
opt.title = false
opt.showcmd = false
opt.showmode = false
opt.cmdheight = 1
opt.switchbuf = "useopen,uselast"
opt.signcolumn = "yes"
opt.ruler = false
opt.wildmenu = true
opt.pumblend = 0
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

-- ~  Search
opt.grepprg = "rg --vimgrep"
opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true
