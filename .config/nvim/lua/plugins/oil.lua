local border = require("util.objects").Border
return {
  "stevearc/oil.nvim",
  opts = {
    default_file_explorer = true,
    columns = { "icon" },
    buf_options = { buflisted = false, bufhidden = "hide" },
    win_options = { list = true },
    delete_to_trash = false,
    skip_confirm_for_simple_edits = false,
    prompt_save_on_select_new_entry = true,
    cleanup_delay_ms = 2000,
    lsp_rename_autosave = true,
    constrain_cursor = "name",
    use_default_keymaps = false,
    keymaps = {
      ["?"] = "actions.show_help",
      ["~"] = "actions.open_cwd",
      ["@"] = "actions.cd",
      ["gH"] = "actions.open({/home/tibor/})",
      ["gs"] = "actions.change_sort",
      ["gx"] = "actions.open_external",
      ["g."] = "actions.toggle_hidden",
      ["g\\"] = "actions.toggle_trash",
      ["h"] = "actions.parent",
      ["l"] = "actions.select",
      ["<CR>"] = "actions.select",
      ["<C-v>"] = "actions.select_vsplit",
      ["<C-x>"] = "actions.select_split",
      ["<C-t>"] = "actions.select_tab",
      ["<C-p>"] = "actions.preview",
      ["<C-f>"] = "actions.preview_scroll_down",
      ["<C-b>"] = "actions.preview_scroll_up",
      ["<C-c>"] = "actions.close",
      ["<C-r>"] = "actions.refresh",
    },
    view_options = { show_hidden = true, is_hidden_file = function(name, _) return vim.startswith(name, ".") end },
    float = {
      padding = 2,
      max_width = math.floor(vim.api.nvim_win_get_width(0) * 0.3),
      max_height = math.floor(vim.api.nvim_win_get_height(0) * 0.4),
      override = function(conf)
        conf.style = "minimal"
        conf.border = border
        return conf
      end,
    },
  },
}
