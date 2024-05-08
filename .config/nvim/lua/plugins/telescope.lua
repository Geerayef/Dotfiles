local border = require("util.objects").BorderSimple
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
      local sorter = require("telescope.sorters")
      telescope.setup({
        defaults = {
          border = { prompt = { 1, 1, 1, 1 }, results = { 1, 1, 1, 1 }, preview = { 1, 1, 1, 1 } },
          borderchars = { prompt = border, results = border, preview = border },
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
            "--trim",
          },
          generic_sorter = sorter.get_generic_fuzzy_sorter,
          file_sorter = sorter.get_fuzzy_file,
          file_ignore_patterns = { "node_modules", ".git" },
          layout_strategy = "vertical",
          layout_config = {
            height = 0.5,
            width = 0.8,
            prompt_position = "top",
            vertical = { preview_height = 0.2, height = 0.4, width = 0.4 },
            horizontal = {
              preview_width = function(_, cols, _) return cols > 200 and math.floor(cols * 0.4) or math.floor(cols * 0.5) end,
            },
          },
        },
        pickers = {
          current_buffer_fuzzy_find = { prompt_title = "Search", layout_config = { width = 0.8 }, skip_empty_lines = true },
          fd = { prompt_title = "Files", find_command = { "fd", "--hidden", "--strip-cwd-prefix", "--type", "f" } },
          buffers = { initial_mode = "normal", sort_lastused = true, sort_mru = true, max_results = 10 },
          oldfiles = { prompt_title = "Old files" },
          help_tags = {
            preview_title = "",
            layout_config = { mirror = true, preview_cutoff = 1, preview_height = 0.6, height = 0.8, width = 0.8 },
          },
          live_grep = { prompt_title = "Grep", max_results = 10, layout_config = { width = 0.8 } },
          grep_string = { preview_title = "", only_sort_text = true, disable_coordinates = true, word_match = "-w" },
          diagnostics = { preview_title = "", theme = "ivy", initial_mode = "normal" },
          lsp_references = {
            preview_title = "",
            theme = "ivy",
            path_display = { tail = true, shorten = 2 },
            disable_coordinates = true,
            jump_type = "vsplit",
            show_line = false,
            trim_text = true,
          },
          colorscheme = { preview_title = "", enable_preview = true, layout_strategy = "horizontal" },
          keymaps = { layout_strategy = "horizontal" },
        },
        extensions = {
          ["ui-select"] = { th.get_dropdown({}) },
          fzf = { fuzzy = true, override_generic_sorter = true, override_file_sorter = true, case_mode = "smart_case" },
        },
      })
      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")
    end,
  },
}
