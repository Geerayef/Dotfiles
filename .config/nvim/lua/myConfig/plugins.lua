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
local packer_bootstrap = ensure_packer()

-- Auto-reload on save
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost packer.lua source <afile> | PackerSync
  augroup end
]])

local status, packer = pcall(require, "packer")
if not status then
  return
end

packer.startup({function(use)
  use ("wbthomason/packer.nvim")
  use ("nvim-lua/plenary.nvim")

  -- LSP
  use ("neovim/nvim-lspconfig")
  use ("williamboman/mason.nvim")
  use ("williamboman/mason-lspconfig.nvim")

  use ({
    "nvim-treesitter/nvim-treesitter",
    run = function()
      local ts_update = require("nvim-treesitter.install").update({ with_sync = true })
      ts_update.setup()
    end,
  })
  use ({
    "nvim-treesitter/nvim-treesitter-textobjects",
    after = "nvim-treesitter",
    requires = "nvim-treesitter/nvim-treesitter"
  })
  use ({ "windwp/nvim-ts-autotag", after = "nvim-treesitter" })

  -- Autocompletion
  use ("hrsh7th/nvim-cmp")
  use ("hrsh7th/cmp-nvim-lsp")
  use ("hrsh7th/cmp-buffer")
  use ("hrsh7th/cmp-path")
  use ("hrsh7th/cmp-cmdline")
  use ("onsails/lspkind.nvim")

  -- Snippets
  use ({ "L3MON4D3/LuaSnip" })
  use ("saadparwaiz1/cmp_luasnip")

  use ({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
  use ({ "nvim-telescope/telescope.nvim", branch = "0.1.x" })
  use ({ "nvim-telescope/telescope-ui-select.nvim" })

  -- Git
  use ("tpope/vim-fugitive")
  use ("tpope/vim-rhubarb")
  use ("lewis6991/gitsigns.nvim")

  use ("lukas-reineke/indent-blankline.nvim")
  use ("numToStr/Comment.nvim")
  use ("tpope/vim-sleuth")
  use ("tpope/vim-surround")
  use ("nvim-tree/nvim-tree.lua")
  use ("nvim-tree/nvim-web-devicons")
  use ("windwp/nvim-autopairs")
  use ("freddiehaddad/feline.nvim")

  use ({
    "SmiteshP/nvim-navic",
    requires = "neovim/nvim-lspconfig"
  })
  use ({
    "SmiteshP/nvim-navbuddy",
    requires = {
        "neovim/nvim-lspconfig",
        "SmiteshP/nvim-navic",
        "MunifTanjim/nui.nvim",
        "numToStr/Comment.nvim",
        "nvim-telescope/telescope.nvim"
    }
  })

  use ("Pocco81/true-zen.nvim")

  -- Themes
  use ("folke/tokyonight.nvim")
  use ("EdenEast/nightfox.nvim")
  use ("shaunsingh/nord.nvim")

  if packer_bootstrap then
    require("packer").sync()
  end
end,
config = {
  display = {
    open_fn = function()
      return require('packer.util').float({ border = 'single' })
    end
  }
}})

if packer_bootstrap then
  print  "=================================="
  print  "    Plugins are being installed"
  print  "    Wait until Packer completes,"
  print  "       then restart nvim"
  print  "=================================="
  
  return
end

