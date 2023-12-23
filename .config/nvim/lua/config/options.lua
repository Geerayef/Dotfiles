local o = vim.opt
local cmd = vim.cmd

F.disable_builtin()

o.shell = "/usr/bin/fish"
o.shortmess:append('I')

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
o.termguicolors = true
o.background = "dark"
o.guicursor = ""
o.cursorline = true
o.cursorlineopt = "number"
o.number = true
o.relativenumber = true
o.showtabline = 1
o.title = false
o.laststatus = 3
o.showcmd = false
o.cmdheight = 0
o.showcmdloc = "statusline"
o.showmode = false
o.showmatch = true
o.switchbuf = "useopen,uselast"
o.signcolumn = "yes"
o.ruler = false
o.pumblend = 0
o.wildmenu = true
o.wildmode = "longest,full"
o.wildoptions = "pum"
o.list = true

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
o.grepprg = "rg --vimgrep --no-heading --smart-case"
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true

-- ~  Chars
o.listchars = {
  tab      = "→ ",
  nbsp     = "␣",
  trail    = "·",
}
o.fillchars = {
  fold      = "·",
  foldopen  = "",
  foldclose = "",
  foldsep   = " ",
  diff      = "╱",
  eob       = " ",
}
