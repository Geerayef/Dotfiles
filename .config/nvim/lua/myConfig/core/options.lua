local opt = vim.opt

vim.g.loaded_netrw = 0
vim.g.loaded_netrwPlugin = 0

opt.guicursor = ""

opt.number = true
opt.relativenumber = true

opt.cursorline = true
opt.cursorlineopt = "number"

opt.tabstop = 4
opt.expandtab = true
opt.smarttab = true
opt.shiftwidth = 4
opt.autoindent = true
opt.breakindent = true
opt.smartindent = true
opt.wrap = true
opt.ruler = true
opt.backspace:append('indent,eol,start')

vim.cmd('filetype plugin indent on')
vim.cmd('syntax enable')

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
opt.updatetime = 100
opt.splitright = true
opt.splitbelow = true
opt.scrolloff = 2
opt.sidescrolloff = 5

-- Functions as options
vim.api.nvim_create_autocmd("FileType", {
  pattern = "*sh",
  callback = function()
    vim.lsp.start({
      name = "bash-language-server",
      cmd = { "bash-language-server", "start" },
    })
  end,
})

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
})
