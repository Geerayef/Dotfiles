local road = require("clrs.road")
local rp = road.palette
return {
  {
    "nanozuki/tabby.nvim",
    event = "VeryLazy",
    config = function()
      local theme = {
        current = {
          fg = rp.lotusYellow["DEFAULT"],
          bg = "NONE",
          style = "bold",
        },
        not_current = {
          fg = rp.charcoal[600],
          bg = "NONE",
          style = "italic",
        },
        fill = { bg = "NONE" },
      }
      require("tabby.tabline").set(function(line)
        return {
          line.tabs().foreach(function(tab)
            local modified = false
            local win_ids = require("tabby.module.api").get_tab_wins(tab.id)
            for _, win_id in ipairs(win_ids) do
              local buf_id = vim.api.nvim_win_get_buf(win_id)
              if buf_id then
                if
                  vim.api.nvim_get_option_value("modified", { buf = buf_id })
                then
                  modified = true
                  break
                end
              end
            end
            local hl = tab.is_current() and theme.current or theme.not_current
            return {
              line.sep("|", hl, theme.fill),
              " ",
              tab.number(),
              " ",
              tab.name(),
              " ",
              modified and S.Icons.ui.dot or " ",
              " ",
              hl = hl,
            }
          end),
        }
      end)
    end,
  },
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    config = function()
      local lualine_require = require("lualine_require")
      lualine_require.require = require
      local icon = S.Icons
      require("lualine").setup({
        options = {
          component_separators = "",
          section_separators = "",
          always_divide_middle = true,
          theme = require("clrs.road.line").setup(rp),
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
            { error = { fg = rp.rustyRed["DEFAULT"] }, warn = { fg = rp.lotusYellow[100] }, info = { fg = rp.jet[600] }, hint = { fg = rp.emerald[400] } },
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
            diff_color = { added = { fg = rp.emerald[400] }, modified = { fg = rp.lotusYellow[100] }, removed = { fg = rp.rustyRed["DEFAULT"] } },
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
  },
}
