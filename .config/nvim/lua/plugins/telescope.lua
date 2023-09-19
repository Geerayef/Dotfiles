return {
  -- ~  Telescope
  {
    "nvim-telescope/telescope.nvim",
    priority = 100,
    branch = "0.1.x",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local status, telescope = pcall(require, "telescope")
      if not status then return end

      local fb_status, fb_actions = pcall(require, "telescope._extensions.file_browser.actions")
      if not fb_status then return end

      local actions_setup, actions = pcall(require, "telescope.actions")
      if not actions_setup then return end

      local themes_setup, themes = pcall(require, "telescope.themes")
      if not themes_setup then return end

      telescope.setup({
        defaults = {
          mappings = {
            i = {
              ["<C-k>"] = actions.move_selection_previous,
              ["<C-j>"] = actions.move_selection_next,
              ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
            },
          },
          initial_mode = "insert",
          selection_strategy = "reset",
          sorting_strategy = "ascending",
          scroll_strategy = "cycle",
          color_devicons = true,
          layout_strategy = "horizontal",
          layout_config = {
            width = 0.95,
            height = 0.85,
            prompt_position = "top",
            horizontal = {
              preview_width = function(_, cols, _)
                if cols > 200 then
                  return math.floor(cols * 0.4)
                else
                  return math.floor(cols * 0.5)
                end
              end,
            },
            vertical = {
              width = 0.9,
              height = 0.85,
            },
            flex = {
              horizontal = {
                preview_width = 0.9,
              },
            },
          },
        },
        pickers = {
          current_buffer_fuzzy_find = {
            preview = true,
            skip_empty_lines = true,
            layout_strategy = "vertical",
            layout_config = {
              prompt_position = "top",
              vertical = {
                width = 0.75,
                height = 0.9,
                preview_height = 0.5,
              },
            },
          },
          find_files = {
            hidden = true,
            find_command = vim.fn.executable "fd" == 1 and { "fd", "--strip-cwd-prefix", "--type", "f" } or nil,
          },
          buffers = {
            sort_lastused = true,
            sort_mru = true,
          },
        },
        extensions = {
          ["ui-select"] = {
            themes.get_dropdown({}),
          },
          file_browser = {
            initial_mode = "normal",
            cwd_to_path = true,
            grouped = true,
            hidden = true,
            depth = 2,
            auto_depth = false,
            hijack_netrw = true,
            use_fd = true,
            git_status = true,
            quiet = true,
            display_stat = { date = true, mode = true },
            mappings = {
              ["i"] = {
                ["<A-c>"] = fb_actions.create,
                ["<S-CR>"] = fb_actions.create_from_prompt,
                ["<A-r>"] = fb_actions.rename,
                ["<A-m>"] = fb_actions.move,
                ["<A-y>"] = fb_actions.copy,
                ["<A-d>"] = fb_actions.remove,
                ["<C-o>"] = fb_actions.open,
                ["<C-g>"] = fb_actions.goto_parent_dir,
                ["<C-e>"] = fb_actions.goto_home_dir,
                ["<C-w>"] = fb_actions.goto_cwd,
                ["<C-t>"] = fb_actions.change_cwd,
                ["<C-f>"] = fb_actions.toggle_browser,
                ["<C-h>"] = fb_actions.toggle_hidden,
                ["<C-s>"] = fb_actions.toggle_all,
                ["<bs>"] = fb_actions.backspace,
              },
              ["n"] = {
                ["c"] = fb_actions.create,
                ["r"] = fb_actions.rename,
                ["m"] = fb_actions.move,
                ["y"] = fb_actions.copy,
                ["d"] = fb_actions.remove,
                ["o"] = fb_actions.open,
                ["g"] = fb_actions.goto_parent_dir,
                ["e"] = fb_actions.goto_home_dir,
                ["w"] = fb_actions.goto_cwd,
                ["t"] = fb_actions.change_cwd,
                ["f"] = fb_actions.toggle_browser,
                ["h"] = fb_actions.toggle_hidden,
                ["s"] = fb_actions.toggle_all,
                ["#"] = actions.toggle_selection,
              },
            },
          },
        },
      })

      _ = require("telescope").load_extension "fzf"
      _ = require("telescope").load_extension "file_browser"
      _ = require("telescope").load_extension "ui-select"
    end
  },
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" }
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make"
  },
  {
    "nvim-telescope/telescope-ui-select.nvim",
  },
  -- {
  --   "nvim-telescope/telescope-hop.nvim",
  -- },
}
