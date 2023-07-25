local opt = vim.opt

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

opt.guicursor = ""

opt.number = true
opt.relativenumber = true

opt.tabstop = 4
opt.softtabstop = -1
opt.shiftwidth = 4
opt.expandtab = true
opt.smarttab = true
opt.smartindent = true
opt.autoindent = true
opt.breakindent = true
opt.wrap = true
opt.backspace:append("indent,eol,start")

vim.cmd("filetype plugin indent on")
vim.cmd("syntax enable")

opt.fileformats = "unix"
opt.complete:remove("i")
opt.completeopt = "menu,menuone,noinsert,noselect"
opt.laststatus = 2
opt.wildmenu = true
opt.nrformats:remove("octal")
-- Delete comment character when joining commented lines
opt.formatoptions:append("j")

opt.ignorecase = true
opt.smartcase = true
opt.hlsearch = true
opt.incsearch = true

opt.termguicolors = true
opt.background = "dark"
opt.signcolumn = "auto"
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

