return {
  "nvim-lualine/lualine.nvim",
  event = "UIEnter",
  init = function()
    vim.g.lualine_laststatus = vim.o.laststatus
    if vim.fn.argc(-1) > 0 then
      vim.o.statusline = " "
    else
      vim.o.laststatus = 0
    end
  end,
  config = function()
    local lualine_require = require("lualine_require")
    lualine_require.require = require
    local palette = require("colors.kanagawa.palette")
    local theme = require("colors.kanagawa.theme")
    local kanagawaline = require("colors.kanagawa.kanagawaline").setup(theme)
    local Icons = require("util.objects").Icons
    require("lualine").setup({
      options = {
        component_separators = "",
        section_separators = "",
        always_divide_middle = true,
        theme = kanagawaline,
        globalstatus = true,
        disabled_filetypes = { statusline = { "dashboard", "alpha", "starter" } },
      },
      sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {
          { function() return "| " end, color = { fg = palette.dragonWhite }, padding = { left = 0 } },
          {
            "mode",
            fmt = function() return Icons.mode end,
            padding = { right = 1 },
            color = function() return { fg = palette.dragonTeal } end,
          },
          { "filetype", icon_only = true, padding = { left = 1, right = 0 } },
          {
            "filename",
            cond = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end,
            path = 0,
            color = { fg = palette.dragonAqua },
            symbols = { modified = Icons.touched, readonly = Icons.lock, unnamed = "[No Name]", newfile = "[New]" },
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
            symbols = { added = Icons.git.added, modified = Icons.git.modified_simple, removed = Icons.git.removed },
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
              error = Icons.diagnostics.error,
              warn = Icons.diagnostics.warn,
              info = Icons.diagnostics.info,
              hint = Icons.diagnostics.hint,
            },
            diagnostics_color = {
              error = { fg = theme.diag.error },
              warn = { fg = theme.diag.warning },
              info = { fg = theme.diag.info },
              hint = { fg = theme.diag.hint },
            },
          },
          { "branch", icon = Icons.git.branch, color = { fg = palette.dragonGreen } },
          { function() return " |" end, color = { fg = palette.dragonWhite }, padding = { right = 0 } },
        },
        lualine_y = {},
        lualine_z = {},
      },
    })
  end,
}
