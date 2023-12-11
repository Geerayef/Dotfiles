local o = vim.opt
local cmd = vim.cmd

F.disable_builtin()

o.shell = "/usr/bin/fish"
-- o.compatible = false

-- ~  File
cmd.syntax("on")
cmd.filetype("plugin", "indent", "on")
o.fileformats = "unix"
o.undofile = true
o.formatoptions:append("j")
o.nrformats:remove("octal")

-- ~  Edit
o.tabstop = 4
o.softtabstop = -1
o.shiftwidth = 4
o.expandtab = true
o.smarttab = true
o.autoindent = true
o.smartindent = true
o.breakindent = true
o.wrap = true
o.complete:remove("i")
o.completeopt = "menu,menuone,noinsert,noselect"
o.virtualedit = "block"

-- ~  UI
o.background = "dark"
o.guicursor = ""
o.cursorline = true
o.cursorlineopt = "number"
o.termguicolors = true
o.number = true
o.relativenumber = true
o.laststatus = 3
o.showtabline = 1
o.title = false
o.showcmd = false
o.showmode = false
o.showmatch = true
o.cmdheight = 0
o.switchbuf = "useopen,uselast"
o.signcolumn = "yes"
o.ruler = false
o.wildmenu = true
o.pumblend = 0
o.wildmode = "longest,full"
o.wildoptions = "pum"

-- ~  Behaviour
o.ttyfast = true
o.lazyredraw = true
o.updatetime = 300
o.belloff = "all"
o.splitright = true
o.splitbelow = true
o.scrolloff = 4
o.sidescrolloff = 4

-- ~  Search
o.grepprg = "rg"
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true
