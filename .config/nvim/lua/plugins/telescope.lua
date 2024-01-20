return {
  {
    "nvim-telescope/telescope.nvim",
    priority = 100,
    branch = "0.1.x",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      local status, telescope = pcall(require, "telescope")
      if not status then
        vim.notify("Telescope cannot be required")
        return
      end
      -- local fb_status, fb_act = pcall(require, "telescope._extensions.file_browser.actions")
      -- if not fb_status then
      --   vim.notify("Telescope File Browser actions cannot be required")
      -- end
      local actions_setup, act = pcall(require, "telescope.actions")
      if not actions_setup then vim.notify("Telescope Actions cannot be required") end
      local themes_setup, themes = pcall(require, "telescope.themes")
      if not themes_setup then vim.notify("Telescope Themes cannot be required") end
      telescope.setup({
        defaults = {
          mappings = {
            i = { ["<C-c>"] = act.close, ["<C-k>"] = act.move_selection_previous, ["<C-j>"] = act.move_selection_next },
            n = { ["<C-c>"] = act.close },
          },
          initial_mode = "insert",
          selection_strategy = "reset",
          sorting_strategy = "ascending",
          scroll_strategy = "cycle",
          color_devicons = true,
          layout_strategy = "horizontal",
          layout_config = {
            height = 0.8,
            width = 0.8,
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
          },
        },
        pickers = {
          current_buffer_fuzzy_find = {
            prompt_title = "Find in buffer",
            previewer = false,
            layout_strategy = "horizontal",
            layout_config = { prompt_position = "bottom", height = 0.5, width = 0.8 },
            skip_empty_lines = true,
          },
          find_files = {
            previewer = false,
            layout_strategy = "vertical",
            layout_config = { height = 0.8, width = 0.4 },
            hidden = true,
            find_command = function()
              if vim.fn.executable("fd") == 1 then return { "fd", "--strip-cwd-prefix", "--type", "f" } end
            end,
          },
          buffers = {
            previewer = false,
            initial_mode = "normal",
            layout_strategy = "vertical",
            layout_config = { height = 0.8, width = 0.4 },
            sort_lastused = true,
            sort_mru = true,
          },
          oldfiles = { previewer = false, layout_strategy = "vertical", layout_config = { height = 0.8, width = 0.5 } },
          colorscheme = { enable_preview = true },
          help_tags = { layout_config = { height = 0.4, width = 0.9 } },
          diagnostics = { theme = "ivy", layout_config = { height = 0.5, width = 0.9 } },
        },
        extensions = {
          ["ui-select"] = { themes.get_dropdown({}) },
          fzf = { fuzzy = true, override_generic_sorter = true, override_file_sorter = true },
          -- file_browser = {
          --   initial_mode = "normal",
          --   cwd_to_path = true,
          --   grouped = true,
          --   hidden = true,
          --   depth = 1,
          --   auto_depth = false,
          --   hijack_netrw = true,
          --   use_fd = true,
          --   git_status = false,
          --   quiet = true,
          --   display_stat = { mode = true, date = false },
          --   mappings = {
          --     ["i"] = {
          --       ["<A-c>"] = fb_actions.create,
          --       ["<S-CR>"] = fb_actions.create_from_prompt,
          --       ["<A-r>"] = fb_actions.rename,
          --       ["<A-m>"] = fb_actions.move,
          --       ["<A-y>"] = fb_actions.copy,
          --       ["<A-d>"] = fb_actions.remove,
          --       ["<C-o>"] = fb_actions.open,
          --       ["<C-g>"] = fb_actions.goto_parent_dir,
          --       ["<C-e>"] = fb_actions.goto_home_dir,
          --       ["<C-w>"] = fb_actions.goto_cwd,
          --       ["<C-t>"] = fb_actions.change_cwd,
          --       ["<C-f>"] = fb_actions.toggle_browser,
          --       ["<C-h>"] = fb_actions.toggle_hidden,
          --       ["<C-s>"] = fb_actions.toggle_all,
          --       ["<bs>"] = fb_actions.backspace,
          --     },
          --     ["n"] = {
          --       ["<A-c>"] = fb_actions.create,
          --       ["<A-r>"] = fb_actions.rename,
          --       ["<A-m>"] = fb_actions.move,
          --       ["<A-y>"] = fb_actions.copy,
          --       ["<A-d>"] = fb_actions.remove,
          --       ["o"] = fb_actions.open,
          --       ["g"] = fb_actions.goto_parent_dir,
          --       ["e"] = fb_actions.goto_home_dir,
          --       ["w"] = fb_actions.goto_cwd,
          --       ["t"] = fb_actions.change_cwd,
          --       ["f"] = fb_actions.toggle_browser,
          --       ["h"] = fb_actions.toggle_hidden,
          --       ["s"] = fb_actions.toggle_all,
          --       ["#"] = actions.toggle_selection,
          --     },
          --   },
          -- },
        },
      })
      _ = require("telescope").load_extension("fzf")
      _ = require("telescope").load_extension("ui-select")
      -- _ = require("telescope").load_extension("file_browser")
    end,
  },
  { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  { "nvim-telescope/telescope-ui-select.nvim" },
  -- { "nvim-telescope/telescope-file-browser.nvim", dependencies = { "nvim-telescope/telescope.nvim" } },
}
