local road = require("clrs.road")
local rp, rb = road.palette, road.base
local i = S.Icons
return {
  "nvim-lualine/lualine.nvim",
  event = "VeryLazy",
  opts = function()
    local lualine_require = require("lualine_require")
    lualine_require.require = require
    require("lualine").setup({
      options = {
        component_separators = "",
        section_separators = "",
        always_divide_middle = true,
        theme = require("clrs.road.line").setup(rp),
        globalstatus = true,
      },
      sections = {
        lualine_a = {},
        lualine_b = {
          { function() return " " end, padding = { left = 13, right = 0 } },
        },
        lualine_c = {
          { function() return "│" end, padding = 0 },
          {
            "filename",
            cond = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end,
            path = 0,
            symbols = {
              modified = i.ui.dot_l,
              readonly = i.ui.lock,
              unnamed = "[Scratch]",
              newfile = "[New]",
            },
          },
          { "searchcount", color = { fg = "String", gui = "bold" } },
          { function() return "%=" end },
        },
        lualine_x = {
          {
            function() return "recording @" .. vim.fn.reg_recording() .. " " end,
            cond = function() return vim.fn.reg_recording() ~= "" end,
          },
          {
            "diagnostics",
            sources = { "nvim_lsp", "nvim_diagnostic" },
            symbols = {
              error = i.diagnostics.error,
              warn = i.diagnostics.warn,
              info = i.diagnostics.info,
              hint = i.diagnostics.hint,
            },
            diagnostics_color = {
              error = { fg = rb.rustyRed },
              warn = { fg = rp.lotusYellow[200] },
              info = { fg = rb.cadetGray },
              hint = { fg = rp.emerald[300] },
            },
          },
          {
            function() return "│" end,
            cond = function()
              return require("grim.git").versioned_p(vim.api.nvim_get_current_buf())
            end,
          },
          {
            "diff",
            cond = function()
              return require("grim.git").versioned_p(vim.api.nvim_get_current_buf())
            end,
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
            colored = true,
            symbols = {
              added = i.git.added_simple,
              modified = i.git.modified_simple_up,
              removed = i.git.removed_simple,
            },
            diff_color = {
              added = { fg = rp.emerald[300] },
              modified = { fg = rp.lotusYellow[200] },
              removed = { fg = rb.rustyRed },
            },
          },
          { "branch", icon = i.git.branch },
        },
        lualine_y = {
          { function() return " " end, padding = { left = 0, right = 3 } },
        },
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
      extensions = { "oil", "quickfix", "man", "fzf", "lazy" },
      tabline = {},
      winbar = {},
    })
  end,
}
