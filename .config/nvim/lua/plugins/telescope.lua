return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = { "Telescope" },
    -- branch = "0.1.x",
    version = false,
    dependencies = {
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        lazy = true,
        build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
      },
      { "nvim-telescope/telescope-ui-select.nvim", lazy = true },
    },
    config = function()
      local telescope = require("telescope")
      local a = require("telescope.actions")
      local th = require("telescope.themes")
      telescope.setup({
        defaults = {
          prompt_prefix = " ",
          selection_caret = " ",
          mappings = {
            i = { ["<C-c>"] = a.close, ["<C-j>"] = a.move_selection_next, ["<C-k>"] = a.move_selection_previous },
            n = { ["<C-c>"] = a.close },
          },
          sorting_strategy = "ascending",
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
            layout_config = { height = 0.8, width = 0.5 },
            hidden = true,
            find_command = function()
              if vim.fn.executable("fd") == 1 then return { "fd", "--strip-cwd-prefix", "--type", "f" } end
            end,
          },
          fd = {
            previewer = false,
            layout_strategy = "vertical",
            layout_config = { height = 0.8, width = 0.5 },
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
          diagnostics = { theme = "ivy", layout_config = { height = 0.5 } },
        },
        extensions = {
          ["ui-select"] = { th.get_dropdown({}) },
          fzf = { fuzzy = true, override_generic_sorter = true, override_file_sorter = true },
        },
      })
    end,
  },
}
