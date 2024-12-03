local M = {}

function M.setup(kt)
  local kanagawaline = {}
  kanagawaline.normal = {
    a = { bg = kt.ui.bg_m3, fg = kt.term[18] },
    b = { bg = kt.ui.bg_m3, fg = kt.term[18] },
    c = { bg = "Normal", fg = kt.ui.fg, gui = "bold" },
  }
  kanagawaline.insert = {
    a = { bg = kt.ui.bg_m3, fg = kt.ui.fg },
    b = { bg = kt.ui.bg_m3, fg = kt.ui.fg },
  }
  kanagawaline.command = {
    a = { bg = kt.ui.bg_m3, fg = kt.ui.fg },
    b = { bg = kt.ui.bg_m3, fg = kt.ui.fg },
  }
  kanagawaline.visual = {
    a = { bg = kt.ui.bg_m3, fg = kt.ui.fg },
    b = { bg = kt.ui.bg_m3, fg = kt.ui.fg },
  }
  kanagawaline.replace = {
    a = { bg = kt.ui.bg_m3, fg = kt.ui.fg },
    b = { bg = kt.ui.bg_m3, fg = kt.ui.fg },
  }
  kanagawaline.inactive = {
    a = { bg = "Normal", fg = kt.ui.bg_p2 },
    b = { bg = "Normal", fg = kt.ui.bg_p2 },
    c = { bg = "Normal", fg = kt.ui.bg_p2 },
  }
  if vim.g.kanagawa_lualine_bold then
    for _, mode in pairs(kanagawaline) do
      mode.a.gui = "bold"
    end
  end

  return kanagawaline
end

return M
