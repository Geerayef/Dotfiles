vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_python3_provider = 0

local o = vim.opt

o.shell = "/usr/bin/fish"
o.shortmess:append("IWSsac")
if vim.fn.executable("rg") == 1 then
  o.grepprg = "rg -. --color=never --no-heading -n --column -H  -S --trim --"
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
o.completeopt = "fuzzy,menuone,noselect,noinsert,popup"
o.showfulltag = true
o.virtualedit = "block"
o.wildmenu = true
o.wildmode = "longest,full,noselect"
o.wildoptions = "pum"
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
o.numberwidth = 8
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
o.colorcolumn = { 80 }
o.ruler = false
o.pumblend = 0
o.pumborder = "single"
o.list = true
o.conceallevel = 2
o.guifont = "Iosevka,IosevkaTerm NFM:h16"
o.linespace = 8
o.winborder = "single"
require("vim._core.ui2").enable({
  enable = true,
  msg = {
    targets = {
      [""] = "msg",
      empty = "cmd",
      bufwrite = "msg",
      confirm = "cmd",
      emsg = "pager",
      echo = "msg",
      echomsg = "msg",
      echoerr = "pager",
      completion = "cmd",
      list_cmd = "pager",
      lua_error = "pager",
      lua_print = "msg",
      progress = "pager",
      rpc_error = "pager",
      quickfix = "msg",
      search_cmd = "cmd",
      search_count = "cmd",
      shell_cmd = "pager",
      shell_err = "pager",
      shell_out = "pager",
      shell_ret = "msg",
      undo = "msg",
      verbose = "pager",
      wildlist = "cmd",
      wmsg = "msg",
      typed_cmd = "cmd",
    },
    cmd = { height = 0.5 },
    dialog = { height = 0.5 },
    msg = { height = 0.3, timeout = 5000 },
    pager = { height = 0.5 },
  },
})

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
o.sessionoptions = "buffers,curdir,folds,tabpages,localoptions"
o.synmaxcol = 256

-- ~ Chars
local ui = GRIM.static.icon.ui
o.listchars = { tab = ui.angle_right_s .. " ", nbsp = ui.space, trail = ui.dot_s }
o.fillchars = {
  fold = ui.dot_l,
  foldopen = ui.arrow_down,
  foldclose = ui.arrow_right,
  foldsep = " ",
  diff = "╱",
  eob = " ",
}
