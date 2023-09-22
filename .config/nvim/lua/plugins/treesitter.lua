return {
  -- ~  TreeSitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    config = function ()
      local treesitter = require("nvim-treesitter.configs")

      treesitter.setup({
        highlight = { enable = true },
        autotag = { enable = true },
        indent = {
          enable = true,
          disable = {
            "python",
            "ocaml"
          }
        },
        incremental_selection = {
          enable = false,
          keymaps = {
            init_selection = "<C-space>",
            node_incremental = "<C-space>",
            scope_incremental = "<C-s>",
            node_decremental = "<C-backspace>",
          },
        },
        ensure_installed = {
          "bash",
          "c",
          "lua",
          "luadoc",
          "ocaml",
          "python",
          "rust",
          "vim",
          "yaml",
        },
        autoinstall = true,
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["aa"] = "@parameter.outer",
              ["ia"] = "@parameter.inner",
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
            },
          },
          move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
              ["]m"] = "@function.outer",
              ["]]"] = "@class.outer",
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]["] = "@class.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[["] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
            },
          },
          swap = {
            enable = true,
            swap_next = {
              ["<leader>a"] = "@parameter.inner",
            },
            swap_previous = {
              ["<leader>A"] = "@parameter.inner",
            },
          },
          lsp_interop = {
            enable = true,
            border = 'none',
            floating_preview_opts = {},
            peek_definition_code = {
              ["<leader>pfd"] = "@function.outer",
              ["<leader>pcd"] = "@class.outer",
            },
          },
        },
      })
    end
  },

  -- ~  TreeSitter text objects
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    event = {
      "BufReadPost",
      "BufNewFile",
    },
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
  },
}
