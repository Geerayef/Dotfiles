vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.did_install_default_menus = 1

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
o.complete:append("f,i,kspell")
o.completeopt = "menuone,noselect,noinsert,fuzzy"
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
o.cursorlineopt = "both"
o.title = false
o.showtabline = 1
o.laststatus = 3
o.showcmd = false
o.cmdheight = 0
o.showmode = false
o.showmatch = true
o.switchbuf = "usetab,uselast,useopen"
o.signcolumn = "yes"
o.colorcolumn = { 80, 96 }
o.ruler = false
o.pumblend = 0
o.list = true
o.conceallevel = 2
o.guifont = "Iosevka,IosevkaTerm NFM:h16"
o.linespace = 8
o.winborder = "single"

-- ~ Behaviour
o.lazyredraw = false
o.updatetime = 100
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
o.synmaxcol = 256

-- ~ Chars
local ui = S.Icons.ui
o.listchars = { tab = ui.angle_right_s .. " ", nbsp = ui.space, trail = ui.dot_s }
o.fillchars = {
  fold = ui.dot_l,
  foldopen = ui.arrow_down,
  foldclose = ui.arrow_right,
  foldsep = " ",
  diff = "╱",
  eob = " ",
}
