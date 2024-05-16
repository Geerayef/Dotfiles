local o = vim.opt

o.shell = "/usr/bin/fish"
o.shortmess:append("Is")
if vim.fn.executable("rg") == 1 then o.grepprg = "rg --unrestricted --vimgrep --no-heading --smart-case --trim" end

-- ~  File
vim.cmd.syntax("on")
vim.cmd.filetype({ args = { "plugin", "indent", "on" } })
o.fileformats = "unix"
o.fileencoding = "utf-8"
o.undofile = true

-- ~  Edit
o.tabstop = 4
o.softtabstop = -1
o.shiftwidth = 4
o.expandtab = true
o.smarttab = true
o.autoindent = true
o.smartindent = true
o.wrap = true
o.linebreak = true
o.breakindent = true
o.showbreak = "- "
o.complete:remove("i")
o.completeopt = "menu,menuone,noinsert,noselect"
o.showfulltag = true
o.virtualedit = "block"
o.backspace = "indent,eol,start"
o.wildmenu = true
o.wildmode = "list:full"
o.wildoptions = "fuzzy,pum"

-- ~  UI
o.termguicolors = true
o.background = "dark"
o.guicursor = "a:block-blinkon0"
o.number = true
o.relativenumber = true
o.cursorline = true
o.cursorlineopt = "number"
o.showtabline = 1
o.title = false
o.laststatus = 3
o.showcmd = false
o.cmdheight = 0
o.showcmdloc = "statusline"
o.showmode = false
o.showmatch = true
o.switchbuf = "useopen,uselast,usetab"
o.signcolumn = "yes"
o.ruler = false
o.pumblend = 0
o.list = true
o.conceallevel = 2

-- ~  Behaviour
o.lazyredraw = false
o.updatetime = 200
o.belloff = "all"
o.splitright = true
o.splitbelow = true
o.scroll = 10
o.scrolloff = 4
o.sidescrolloff = 4
o.foldenable = true
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true
o.inccommand = "split"

-- ~  Chars
o.listchars = { tab = "→ ", nbsp = "␣", trail = "·" }
o.fillchars = { fold = "·", foldopen = "", foldclose = "", foldsep = " ", diff = "╱", eob = " " }
