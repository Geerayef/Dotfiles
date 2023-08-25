local status, nightfox = pcall(require, "nightfox")
if not status then
  print("Colorscheme not found!")
  return
end

nightfox.setup({
  options = {
    styles = {
      constants = "bold"
    },
    inverse = {
      match_paren = false,
      visual = true,
      search = true,
    },
    colorblind = {
      enable = true,
      severity = {
        deutan = 1
      }
    }
  }
})

-- tokyonight.setup {
--     style = "moon",
--     lualine_bold = true,
--     styles = {
--         keywords = { italic = false },
--     }
-- }

-- vim.g.nord_borders = true
-- vim.g.nord_italic = false

-- nightfox / duskfox / nordfox / terafox
vim.cmd("colorscheme duskfox")
