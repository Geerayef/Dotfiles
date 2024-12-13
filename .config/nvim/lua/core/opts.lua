vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_python3_provider = 0

local o = vim.opt

o.shell = "/usr/bin/fish"
o.shortmess:append("IWSsac")
if vim.fn.executable("rg") == 1 then
  o.grepprg =
    "rg --vimgrep --hidden --color=never --no-heading --line-number --column --with-filename  --smart-case --trim --"
end

-- ~ File
vim.cmd.syntax("on")
vim.cmd.filetype({ args = { "plugin", "indent", "on" } })
o.fileformats = "unix"
o.fileencoding = "utf-8"
o.undofile = true

-- ~ Edit
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
o.showbreak = "  "
o.complete:append("i,kspell")
o.completeopt = "menu,menuone,noselect"
o.showfulltag = true
o.virtualedit = "block"
o.wildmenu = true
o.wildmode = "list:longest,list:full"
o.wildoptions = "fuzzy,pum"
o.wildignore:append({
  ".javac",
  "node_modules",
  "*.pyc",
  ".aux",
  ".out",
  ".toc",
  ".o",
  ".obj",
  ".dll",
  ".exe",
  ".so",
  ".a",
  ".lib",
  ".pyo",
  ".pyd",
  ".swp",
  ".swo",
  ".class",
  ".DS_Store",
  ".git",
  ".hg",
  ".orig",
})
o.suffixesadd:append({ ".java", ".rs" })

-- ~ UI
o.termguicolors = true
o.background = "dark"
o.number = true
o.relativenumber = true
o.cursorline = true
o.cursorlineopt = "number"
o.title = false
o.showtabline = 1
o.laststatus = 3
o.showcmd = false
o.cmdheight = 0
o.showmode = false
o.showmatch = true
o.switchbuf = "usetab,uselast,useopen"
o.signcolumn = "yes"
o.ruler = false
o.pumblend = 0
o.list = true
o.conceallevel = 2

-- ~ Behaviour
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
o.inccommand = "split"
o.hlsearch = true
o.incsearch = true
o.mouse = ""
o.sessionoptions = "buffers,curdir,folds,help,tabpages,localoptions"

-- ~ Chars
local ui = S.Icons.ui
o.listchars = { tab = ui.arrow_right .. " ", nbsp = ui.space, trail = ui.dot_s }
o.fillchars = {
  fold = ui.dot_l,
  foldopen = ui.angle_down_s,
  foldclose = ui.angle_right_s,
  foldsep = " ",
  diff = "╱",
  eob = " ",
}
