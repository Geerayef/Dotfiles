return {
  "ibhagwan/fzf-lua",
  cmd = "FzfLua",
  config = function()
    local border = O.Border
    local actions = require("fzf-lua.actions")
    return {
      winopts = {
        height = 0.4,
        width = 0.4,
        border = border,
        title = "",
        preview = {
          default = "bat",
          border = "border",
          wrap = "nowrap",
          hidden = "hidden",
          layout = "vertical",
          title = false,
          scrollbar = false,
          winopts = { cursorlineopt = "number" },
        },
      },
      previewers = { man = { cmd = "man %s | col -bx" } },
      files = {
        previewer = "bat",
        prompt = " ",
        path_shorten = 1,
        cwd_header = false,
        cwd_prompt = false,
      },
      git = {
        files = { prompt = " " },
        status = { prompt = " " },
        commits = { prompt = " " },
        bcommits = { prompt = " " },
        branches = { prompt = " " },
        tags = { prompt = " " },
        stash = { prompt = " " },
      },
      grep = { prompt = " ", input_prompt = "❯ " },
      args = { prompt = " " },
      oldfiles = { prompt = " " },
      buffers = { prompt = " " },
      tabs = { prompt = " ", tab_title = "" },
      lines = { previewer = "builtin", prompt = " " },
      blines = { previewer = "builtin", prompt = " " },
      tags = { prompt = " " },
      btags = { prompt = " " },
      colorschemes = { prompt = " " },
      awesome_colorschemes = { prompt = " " },
      keymaps = { prompt = " " },
      quickfix_stack = { prompt = " ", marker = " " },
      lsp = {
        prompt_postfix = " ",
        code_actions = { prompt = " ", previewer = "codeaction" },
        finder = { prompt = " " },
      },
      diagnostics = {
        prompt = " ",
        signs = {
          ["Error"] = { text = "", texthl = "DiagnosticError" },
          ["Warn"] = { text = "", texthl = "DiagnosticWarn" },
          ["Info"] = { text = "", texthl = "DiagnosticInfo" },
          ["Hint"] = { text = "󰌵", texthl = "DiagnosticHint" },
        },
      },
      complete_path = { cmd = nil, complete = { ["default"] = actions.complete } },
      complete_file = { cmd = nil, winopts = { preview = { hidden = "hidden" } } },
    }
  end,
}
