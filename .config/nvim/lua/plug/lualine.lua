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
      -- stylua: ignore start
      sections = {
        lualine_a = {},
        lualine_b = { { function() return "          " end, color = { bg = "Normal" }, padding = { left = 0, right = 0 } } },
        lualine_c = {
          { function() return "|" end, color = { fg = palette.lotusWhite0 }, padding = { left = 1 } },
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
          { "searchcount", color = { fg = palette.dragonYellow, gui = "bold" } },
          { function() return "%=" end },
        },
        lualine_x = {
          {
            function() return "recording @" .. vim.fn.reg_recording() .. " " end,
            cond = function() return (vim.fn.reg_recording() ~= "") and true or false end
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
              warn = { fg = theme.diag.warn },
              info = { fg = theme.diag.info },
              hint = { fg = theme.diag.hint },
            },
          },
          {
            function () return "|" end,
            cond = function () return F.IsBufInRepo(0) end,
            color = { bg = "Normal" }, padding = { left = 1, right = 1 }
          },
          {
            "diff",
            cond = function() return F.IsBufInRepo(0) end,
            source = function()
              local g = vim.b.gitsigns_status_dict
              if g then return { added = g.added, modified = g.changed, removed = g.removed } end
            end,
            colored = true,
            symbols = { added = icon.git.added, modified = icon.git.modified_simple, removed = icon.git.removed },
            diff_color = { added = { fg = theme.vcs.added }, modified = { fg = theme.vcs.changed }, removed = { fg = theme.vcs.removed } },
          },
          { "branch", icon = icon.git.branch, color = { fg = palette.dragonGreen } },
        },
        lualine_y = {},
        lualine_z = {},
      },
      inactive_sections = { lualine_a = {}, lualine_b = { "filename" }, lualine_c = {}, lualine_x = {}, lualine_y = {}, lualine_z = {} },
      extensions = { "oil", "quickfix", "man" },
      -- stylua: ignore end
    })
  end,
}
