return {
  "nvim-lualine/lualine.nvim",
  event = "VeryLazy",
  config = function()
    local lualine_require = require("lualine_require")
    lualine_require.require = require
    local palette = require("clrs.kanagawa.palette")
    local theme = require("clrs.kanagawa.theme")
    local kanagawaline = require("clrs.kanagawa.kanagawaline").setup(theme)
    local icon = S.Icons
    require("lualine").setup({
      options = {
        component_separators = "",
        section_separators = "",
        always_divide_middle = true,
        theme = kanagawaline,
        globalstatus = true,
      },
      sections = {
        lualine_a = {},
        lualine_b = {
          {
            function() return "      " end,
            color = { bg = "Normal" },
            padding = { left = 0, right = 0 },
          },
          {
            "mode",
            fmt = function() return F.VimMode(false) end,
            color = { fg = palette.sumiInk0, bg = palette.lotusTeal1 },
            padding = { left = 1, right = 1 },
          },
        },
        lualine_c = {
          {
            function() return "|" end,
            color = { fg = palette.lotusWhite0 },
            padding = { left = 1 },
          },
          {
            "filename",
            cond = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end,
            path = 0,
            symbols = {
              modified = icon.ui.dot_l,
              readonly = icon.ui.lock,
              unnamed = "[Scratch]",
              newfile = "[New]",
            },
          },
          { function() return "%=" end },
        },
        lualine_x = {
          {
            function()
              local reg = vim.fn.reg_recording()
              return "recording @" .. reg .. " "
            end,
            cond = function()
              local r = vim.fn.reg_recording()
              if r ~= "" then
                return true
              else
                return false
              end
            end,
          },
          {
            "diff",
            cond = function() return F.IsBufInRepo(0) end,
            source = function()
              local g = vim.b.gitsigns_status_dict
              if g then
                return {
                  added = g.added,
                  modified = g.changed,
                  removed = g.removed,
                }
              end
            end,
            symbols = {
              added = icon.git.added,
              modified = icon.git.modified_simple,
              removed = icon.git.removed,
            },
            colored = true,
            diff_color = {
              added = { fg = theme.vcs.added },
              modified = { fg = theme.vcs.changed },
              removed = { fg = theme.vcs.removed },
            },
          },
          {
            "diagnostics",
            sources = { "nvim_lsp", "nvim_diagnostic" },
            symbols = {
              error = icon.diagnostics.error,
              warn = icon.diagnostics.warn,
              info = icon.diagnostics.info,
              hint = icon.diagnostics.hint,
            },
            diagnostics_color = {
              error = { fg = theme.diag.error },
              warn = { fg = theme.diag.warning },
              info = { fg = theme.diag.info },
              hint = { fg = theme.diag.hint },
            },
          },
          {
            "branch",
            icon = icon.git.branch,
            color = { fg = palette.dragonGreen },
          },
        },
        lualine_y = {},
        lualine_z = {},
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = { "filename" },
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
      },
      extensions = { "oil", "quickfix", "man" },
    })
  end,
}
