return {
  "nvim-lualine/lualine.nvim",
  event = "VeryLazy",
  config = function()
    local lualine_require = require("lualine_require")
    lualine_require.require = require
    -- local kp = require("clrs.kanagawa.palette")
    local kt = require("clrs.kanagawa.theme")
    local kl = require("clrs.kanagawa.line").setup(kt)
    local icon = S.Icons
    require("lualine").setup({
      options = {
        component_separators = "",
        section_separators = "",
        always_divide_middle = true,
        theme = kl,
        globalstatus = true,
      },
      -- stylua: ignore start
      sections = {
        lualine_a = {},
        lualine_b = { { function() return " " end, color = { bg = "NONE" }, padding = { left = 10, right = 0 } } },
        lualine_c = {
          { function() return "│" end, color = { bg = "NONE" }, padding = 0 },
          {
            "filename",
            cond = function() return vim.fn.empty(vim.fn.expand("%:t")) ~= 1 end,
            path = 0,
            symbols = { modified = icon.ui.dot_l, readonly = icon.ui.lock, unnamed = "[Scratch]", newfile = "[New]" },
          },
          { "searchcount", color = { fg = "String", gui = "bold" } },
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
            symbols =
              { error = icon.diagnostics.error, warn = icon.diagnostics.warn, info = icon.diagnostics.info, hint = icon.diagnostics.hint },
            diagnostics_color =
              { error = { fg = kt.diag.error }, warn = { fg = kt.diag.warn }, info = { fg = kt.diag.info }, hint = { fg = kt.diag.hint } },
          },
          {
            function() return "│" end,
            cond = function() return F.IsBufInRepo(0) end,
            color = { bg = "NONE" }
          },
          {
            "diff",
            cond = function() return F.IsBufInRepo(0) end,
            source = function()
              local g = vim.b.gitsigns_status_dict
              if g then return { added = g.added, modified = g.changed, removed = g.removed } end
            end,
            colored = true,
            symbols =    { added = icon.git.added_simple, modified = icon.git.modified_simple_up, removed = icon.git.removed_simple },
            diff_color = { added = { fg = kt.vcs.added }, modified = { fg = kt.vcs.modified }, removed = { fg = kt.vcs.removed } },
          },
          { "branch", icon = icon.git.branch },
        },
        lualine_y = { { function() return " " end, color = { bg = "NONE" }, padding = { left = 0, right = 3 } } },
        lualine_z = {},
      },
      inactive_sections = { lualine_a = {}, lualine_b = { "filename" }, lualine_c = {}, lualine_x = {}, lualine_y = {}, lualine_z = {} },
      extensions = { "oil", "quickfix", "man" },
      -- stylua: ignore end
    })
  end,
}
