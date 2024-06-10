local border = S.BorderSimple
return {
  {
    "nvim-telescope/telescope.nvim",
    cmd = { "Telescope" },
    branch = "0.1.x",
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
            "--smart-case",
            "--color=never",
            "--with-filename",
            "--no-heading",
            "--line-number",
            "--column",
            "--trim",
          },
          generic_sorter = function() return require("telescope").extensions.fzf.native_fzf_sorter end,
          file_sorter = function() return require("telescope").extensions.fzf.native_fzf_sorter end,
          layout_strategy = "vertical",
          layout_config = {
            height = 0.4,
            width = 0.8,
            prompt_position = "top",
            vertical = { preview_height = 0.0, height = 0.4, width = 0.4 },
            horizontal = {
              preview_width = function(_, cols, _) return cols > 200 and math.floor(cols * 0.4) or math.floor(cols * 0.5) end,
            },
          },
        },
        pickers = {
          fd = {
            prompt_title = "Files",
            find_command = { "fd", "-H", "-t", "f", "--strip-cwd-prefix=always", "--color=never", "--prune" },
          },
          grep_string = {
            preview_title = "",
            initial_mode = "normal",
            layout_config = { width = 0.8 },
            only_sort_text = true,
            shorten_path = true,
            disable_coordinates = true,
            word_match = "-w",
          },
          lsp_references = {
            prompt_title = "",
            preview_title = "",
            theme = "ivy",
            borderchars = { preview = border },
            initial_mode = "normal",
            layout_config = { preview_width = 0.7 },
            path_display = { tail = true, shorten = 2 },
            show_line = false,
            trim_text = true,
          },
          diagnostics = {
            prompt_title = "",
            preview_title = "",
            theme = "ivy",
            borderchars = { preview = border },
            initial_mode = "normal",
          },
          help_tags = { preview_title = "", theme = "ivy", borderchars = { preview = border } },
          current_buffer_fuzzy_find = { prompt_title = "Search", layout_config = { width = 0.8 }, skip_empty_lines = true },
          live_grep = { prompt_title = "Grep", layout_config = { width = 0.8 } },
          buffers = { initial_mode = "normal", sort_lastused = true, sort_mru = true, max_results = 10 },
          oldfiles = { prompt_title = "Old files" },
          colorscheme = { preview_title = "", enable_preview = true, layout_strategy = "horizontal" },
          keymaps = { layout_strategy = "horizontal" },
        },
        extensions = {
          ["ui-select"] = { initial_mode = "normal" },
          fzf = { fuzzy = true, override_generic_sorter = true, override_file_sorter = true, case_mode = "smart_case" },
        },
      })
      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")
    end,
  },
}
