return {
  "nvim-lualine/lualine.nvim",
  event = "VeryLazy",
  config = function()
    local lualine_require = require("lualine_require")
    lualine_require.require = require
    local palette = require("colors.kanagawa.palette")
    local theme = require("colors.kanagawa.theme")
    local kanagawaline = require("colors.kanagawa.kanagawaline").setup(theme)
    local icon = O.Icons
    require("lualine").setup({
      options = {
        component_separators = "",
        section_separators = "",
        always_divide_middle = true,
        theme = kanagawaline,
        globalstatus = true,
        disabled_filetypes = { statusline = {} },
      },
      sections = {
        lualine_a = {
          { function() return "| " end, color = { fg = palette.dragonWhite, bg = "Normal" }, padding = { left = 0 } },
          {
            "mode",
            fmt = function() return " " .. icon.mode .. " " .. F.GetViMode(false) end,
            padding = { right = 1 },
            color = function() return { fg = palette.dragonBlack0 } end,
          },
        },
        lualine_b = {},
        lualine_c = {
          { function() return " |" end, color = { fg = palette.dragonWhite }, padding = { left = 0 } },
          { "filetype", icon_only = true, padding = { left = 1, right = 0 } },
          {
            "filename",
            cond = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end,
            path = 0,
            color = { fg = palette.dragonAqua },
            symbols = { modified = icon.touched, readonly = icon.lock, unnamed = "[No Name]", newfile = "[New]" },
          },
          { function() return "%=" end },
        },
        lualine_x = {
          {
            "diff",
            cond = function()
              local filepath = vim.fn.expand("%:p:h")
              local gitdir = vim.fn.finddir(".git", filepath .. ";")
              return gitdir and #gitdir > 0 and #gitdir < #filepath
            end,
            source = function()
              local g = vim.b.gitsigns_status_dict
              if g then return { added = g.added, modified = g.changed, removed = g.removed } end
            end,
            symbols = { added = icon.git.added, modified = icon.git.modified_simple, removed = icon.git.removed },
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
          { "branch", icon = icon.git.branch, color = { fg = palette.dragonGreen } },
        },
        lualine_y = {},
        lualine_z = {
          { function() return " |" end, color = { fg = palette.dragonWhite, bg = "Normal" }, padding = { right = 0 } },
        },
      },
    })
  end,
}
