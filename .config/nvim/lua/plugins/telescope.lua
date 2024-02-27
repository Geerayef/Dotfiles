return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = { "Telescope" },
    version = false,
    dependencies = {
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
      },
      { "nvim-telescope/telescope-ui-select.nvim" },
    },
    config = function()
      local telescope = require("telescope")
      local a = require("telescope.actions")
      local th = require("telescope.themes")
      local sorters = require("telescope.sorters")
      telescope.setup({
        defaults = {
          results_title = "",
          prompt_prefix = " ",
          selection_caret = " ",
          mappings = {
            i = { ["<C-c>"] = a.close, ["<C-j>"] = a.move_selection_next, ["<C-k>"] = a.move_selection_previous },
            n = { ["<C-c>"] = a.close },
          },
          sorting_strategy = "ascending",
          vimgrep_arguments = {
            "rg",
            "-L",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
          },
          generic_sorter = sorters.get_generic_fuzzy_sorter,
          file_sorter = sorters.get_fuzzy_file,
          file_ignore_patterns = { "node_modules", ".git" },
          layout_config = {
            height = 0.8,
            width = 0.8,
            prompt_position = "top",
            horizontal = {
              preview_width = function(_, cols, _) return cols > 200 and math.floor(cols * 0.4) or math.floor(cols * 0.5) end,
            },
          },
        },
        pickers = {
          current_buffer_fuzzy_find = {
            prompt_title = "Search buffer",
            previewer = false,
            layout_strategy = "vertical",
            layout_config = { prompt_position = "bottom", height = 0.5, width = 0.8 },
            skip_empty_lines = true,
          },
          fd = {
            theme = "dropdown",
            previewer = false,
            layout_config = { height = 0.4, width = 0.4 },
            hidden = true,
            find_command = function()
              if vim.fn.executable("fd") == 1 then return { "fd", "--strip-cwd-prefix", "--type", "f" } end
            end,
          },
          buffers = {
            theme = "dropdown",
            initial_mode = "normal",
            previewer = false,
            layout_config = { height = 0.4, width = 0.4 },
            sort_lastused = true,
            sort_mru = true,
          },
          oldfiles = { theme = "dropdown", previewer = false, layout_config = { height = 0.4, width = 0.4 } },
          colorscheme = { preview_title = "", enable_preview = true },
          help_tags = { preview_title = "", layout_config = { height = 0.5, width = 0.9 } },
          live_grep = { preview_title = "", max_results = 20, layout_config = { height = 0.5, width = 0.9 } },
          grep_string = {
            preview_title = "",
            only_sort_text = true,
            word_match = "-w",
            layout_config = { height = 0.5, width = 0.9 },
          },
          diagnostics = { preview_title = "", theme = "ivy", initial_mode = "normal", layout_config = { height = 0.5 } },
          lsp_references = {
            preview_title = "",
            theme = "cursor",
            layout_config = { width = 0.5, height = 0.3 },
            path_display = { tail = true, shorten = 2 },
            disable_coordinates = true,
            show_line = false,
            trim_text = true,
          },
        },
        extensions = {
          ["ui-select"] = { th.get_dropdown({}) },
          fzf = { fuzzy = true, override_generic_sorter = true, override_file_sorter = true },
        },
      })
      telescope.load_extension("ui-select")
      telescope.load_extension("fzf")
    end,
  },
}
