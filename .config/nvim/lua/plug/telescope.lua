local border = S.BorderSimple
return {
  { "nvim-telescope/telescope-ui-select.nvim", lazy = true },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
    lazy = true,
  },
  {
    "nvim-telescope/telescope.nvim",
    cmd = { "Telescope" },
    branch = "0.1.x",
    config = function()
      local telescope = require("telescope")
      local a = require("telescope.actions")
      telescope.setup({
        defaults = {
          border = {
            prompt = { 1, 1, 1, 1 },
            results = { 1, 1, 1, 1 },
            preview = { 1, 1, 1, 1 },
          },
          borderchars = { prompt = border, results = border, preview = border },
          results_title = "",
          prompt_prefix = S.Icons.ui.angle_right_l .. " ",
          selection_caret = S.Icons.ui.triangle_right_s .. " ",
          sorting_strategy = "ascending",
          mappings = {
            i = {
              ["<C-c>"] = a.close,
              ["<C-j>"] = a.move_selection_next,
              ["<C-k>"] = a.move_selection_previous,
            },
            n = { ["<C-c>"] = a.close },
          },
          layout_strategy = "vertical",
          layout_config = {
            prompt_position = "top",
            vertical = {
              height = 0.4,
              width = function(_, cols, _)
                return cols > 100 and math.floor(cols * 0.4)
                  or math.floor(cols * 0.8)
              end,
              preview_height = 0.0,
            },
            horizontal = {
              height = 0.4,
              width = 0.8,
              preview_width = function(_, cols, _)
                return cols > 200 and math.floor(cols * 0.4)
                  or math.floor(cols * 0.5)
              end,
            },
          },
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
        },
        pickers = {
          fd = {
            prompt_title = "Files",
            find_command = {
              "fd",
              "-H",
              "-t",
              "f",
              "--strip-cwd-prefix=always",
              "--color=never",
              "--prune",
            },
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
          lsp_definitions = {
            prompt_title = "",
            preview_title = "",
            theme = "ivy",
            borderchars = { preview = border },
            initial_mode = "normal",
            layout_config = { preview_width = 0.6 },
            path_display = { tail = true, shorten = 2 },
            show_line = false,
            trim_text = true,
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
          lsp_dynamic_workspace_symbols = {
            prompt_title = "Symbols: workspace",
            preview_title = "",
            symbol_width = 32,
            layout_config = { width = 0.6 },
          },
          lsp_document_symbols = {
            prompt_title = "Symbols: buffer",
            preview_title = "",
            symbol_width = 32,
            layout_config = { width = 0.6 },
          },
          diagnostics = {
            prompt_title = "",
            preview_title = "",
            theme = "ivy",
            borderchars = { preview = border },
            initial_mode = "normal",
          },
          help_tags = {
            preview_title = "",
            theme = "ivy",
            borderchars = { preview = border },
          },
          current_buffer_fuzzy_find = {
            prompt_title = "Search",
            layout_config = { width = 0.8 },
            skip_empty_lines = true,
          },
          live_grep = {
            prompt_title = "grep CWD",
            layout_config = { width = 0.8 },
          },
          buffers = {
            initial_mode = "normal",
            sort_lastused = true,
            sort_mru = true,
            max_results = 10,
            mappings = { n = { ["x"] = a.delete_buffer } },
          },
          oldfiles = { prompt_title = "Recent" },
          colorscheme = {
            preview_title = "",
            enable_preview = true,
            layout_strategy = "horizontal",
          },
          keymaps = { layout_strategy = "horizontal" },
          highlights = { layout_strategy = "horizontal" },
          builtin = { prompt_title = "" },
        },
        extensions = {
          ["ui-select"] = { initial_mode = "normal" },
          fzf = {
            fuzzy = true,
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
          },
        },
      })
      telescope.load_extension("fzf")
      telescope.load_extension("ui-select")
    end,
  },
}
