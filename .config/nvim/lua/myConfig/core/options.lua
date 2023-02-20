local opt = vim.opt

opt.guicursor = ""

opt.number = true
opt.relativenumber = true

opt.tabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.autoindent = true
opt.breakindent = true
opt.smartindent = true
opt.wrap = false

opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = false
opt.incsearch = true

opt.termguicolors = true
opt.background = "dark"
opt.signcolumn = "yes"

opt.backspace = "indent,eol,start"
opt.undofile = true

opt.mouse = "a"

opt.updatetime = 50

opt.completeopt = "menu,menuone,noselect"

opt.splitright = true
opt.splitbelow = true

opt.scrolloff = 8

-- Ocaml support integration
opt.runtimepath:prepend( "/home/tibor/.opam/defaults/share/ocp-indent/vim" )
vim.g.opamshare = vim.fn.substitute(vim.fn.system("opam var share"),'\n$',"","")
opt.runtimepath:append( vim.g.opamshare .. "/merlin/vim")
