local M = {}
local kanagawaline = {}

M.setup = function(theme)
  kanagawaline.normal = {
    a = { bg = theme.syn.fun, fg = theme.ui.bg_m3 },
    b = { bg = theme.diff.change, fg = theme.syn.fun },
    c = { bg = "Normal", fg = theme.ui.fg, gui = "bold" },
    -- c = { bg = theme.ui.bg_p1, fg = theme.ui.fg },
  }

  kanagawaline.insert = {
    a = { bg = theme.diag.ok, fg = theme.ui.bg },
    b = { bg = theme.ui.bg, fg = theme.diag.ok },
  }

  kanagawaline.command = {
    a = { bg = theme.syn.operator, fg = theme.ui.bg },
    b = { bg = theme.ui.bg, fg = theme.syn.operator },
  }

  kanagawaline.visual = {
    a = { bg = theme.syn.keyword, fg = theme.ui.bg },
    b = { bg = theme.ui.bg, fg = theme.syn.keyword },
  }

  kanagawaline.replace = {
    a = { bg = theme.syn.constant, fg = theme.ui.bg },
    b = { bg = theme.ui.bg, fg = theme.syn.constant },
  }

  kanagawaline.inactive = {
    a = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
    b = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim, gui = "bold" },
    c = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
  }

  if vim.g.kanagawa_lualine_bold then
    for _, mode in pairs(kanagawaline) do
      mode.a.gui = "bold"
    end
  end
  return kanagawaline
end

return M
