-- auto install packer if not installed
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
    vim.cmd([[packadd packer.nvim]])
    return true
  end
  return false
end
local packer_bootstrap = ensure_packer() -- true if packer was just installed

-- Auto-reload on save
vim.cmd([[ 
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins-setup.lua source <afile> | PackerSync
  augroup end
]])

local status, packer = pcall(require, "packer")
if not status then
  return
end

packer.startup(function(use)
  use ("wbthomason/packer.nvim")

  -- Themes
  use ("CantoroMC/ayu-nvim")
  use ("tiagovla/tokyodark.nvim")
  -- use ("navarasu/onedark.nvim")
  -- use ({ "projekt0n/github-nvim-theme", branch = "0.0.x" })
  -- use ("JoosepAlviste/palenightfall.nvim")

  use ("nvim-lua/plenary.nvim")

  -- Mason - manage & install lsp servers
  use ("williamboman/mason.nvim")
  use ("williamboman/mason-lspconfig.nvim")
  use ("neovim/nvim-lspconfig")

  -- Configuring lsp servers
  use ({ "j-hui/fidget.nvim", after = "nvim-lspconfig" })

  -- Autocompletion
  use ("hrsh7th/nvim-cmp")
  use ("hrsh7th/cmp-buffer")
  use ("hrsh7th/cmp-path")
  use ("hrsh7th/cmp-nvim-lsp")
  use ("onsails/lspkind.nvim")

  -- Snippets
  use ("L3MON4D3/LuaSnip")
  use ("rafamadriz/friendly-snippets")
  use ("saadparwaiz1/cmp_luasnip")

  -- Treesitter
  use ({
    "nvim-treesitter/nvim-treesitter",
    run = function()
      local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
      ts_update.setup()
    end,
  })

  use ({ "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-treesitter", })

  -- Telescope - Fuzzy finder
  use ({ "nvim-telescope/telescope-fzf-native.nvim", run = "make"})
  use ({ "nvim-telescope/telescope.nvim", branch = "0.1.x" })
  use ({ "nvim-telescope/telescope-ui-select.nvim" })

  -- Git related plugins
  use ("tpope/vim-fugitive")
  use ("tpope/vim-rhubarb")
  use ("lewis6991/gitsigns.nvim")

  use ({ "windwp/nvim-ts-autotag", after = "nvim-treesitter" })
  use ("lukas-reineke/indent-blankline.nvim")
  use ("numToStr/Comment.nvim")
  use ("tpope/vim-sleuth")
  use ("tpope/vim-surround")
  use ("nvim-tree/nvim-tree.lua")
  use ("nvim-tree/nvim-web-devicons")
  use ("windwp/nvim-autopairs")
  use ({
    "nvim-lualine/lualine.nvim",
    requires = {
      "nvim-tree/nvim-web-devicons",
      opt = true
    }
  })

  -- Programming Languages
  use ("simrat39/rust-tools.nvim")
  use ("rust-lang/rust.vim")

  if packer_bootstrap then
    require("packer").sync()
  end
end)

if packer_bootstrap then
  print "=================================="
  print "    Plugins are being installed"
  print "    Wait until Packer completes,"
  print "       then restart nvim"
  print "=================================="
  return
end

