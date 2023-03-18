local opt = vim.opt

opt.guicursor = ""

opt.number = true
opt.relativenumber = true

opt.tabstop = 4
opt.expandtab = true
opt.smarttab = true
opt.shiftwidth = 4
opt.autoindent = true
opt.breakindent = true
opt.smartindent = true
opt.wrap = false
opt.ruler = true
opt.backspace:append('indent,eol,start')

if vim.fn.has('autocmd') then
  vim.cmd('filetype plugin indent on')
end

if vim.fn.has('syntax') and not vim.g.syntax_on then
  vim.cmd('syntax enable')
end

opt.completeopt = "menu,menuone,noinsert,noselect"
opt.complete:remove('i')
opt.laststatus = 2
opt.showcmd = true
opt.wildmenu = true
opt.nrformats:remove('octal')
-- Delete comment character when joining commented lines
opt.formatoptions:append('j')

opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = false
opt.incsearch = true

opt.termguicolors = true
opt.background = "dark"
opt.signcolumn = "yes"
opt.undofile = true
opt.display:append('lastline')

opt.mouse = "a"
opt.updatetime = 50
opt.splitright = true
opt.splitbelow = true
opt.scrolloff = 2
opt.sidescrolloff = 5

