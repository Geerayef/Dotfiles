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
o.breakindentopt = "list:-1"
o.showbreak = " - "
o.complete:remove("i")
o.completeopt = "menu,menuone,noinsert,noselect"
o.showfulltag = true
o.virtualedit = "block"
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
o.showcmd = true
o.cmdheight = 1
o.showcmdloc = "last"
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
o.foldmethod = "marker"
o.smartcase = true
o.ignorecase = true
o.hlsearch = true
o.incsearch = true
o.inccommand = "split"
o.mouse = ""

-- ~  Chars
local ui = S.Icons.ui
o.listchars = { tab = ui.arrow_r .. " ", nbsp = ui.space, trail = ui.dot_s }
o.fillchars = { fold = ui.dot_s, foldopen = ui.angle_d, foldclose = ui.angle_r, foldsep = " ", diff = "╱", eob = " " }
