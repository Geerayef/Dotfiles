local last_active_win = {}
local devicons = require("nvim-web-devicons")

vim.api.nvim_create_autocmd("WinEnter", {
  callback = function()
    last_active_win[vim.api.nvim_get_current_tabpage()] = vim.api.nvim_get_current_win()
  end,
})

local get_buf_name = function(buf)
  local buf_name = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t")
  local buf_type = vim.bo[buf].buftype
  local buf_ft = vim.bo[buf].filetype
  if buf_type == "help" then
    return "Help: " .. buf_name
  elseif buf_ft == "oil" then
    return "Oil: " .. (buf_name ~= "" and buf_name or vim.fn.getcwd())
  elseif buf_type == "terminal" then
    return "Terminal"
  elseif buf_type == "quickfix" then
    return "Quickfix"
  elseif buf_name == "" then
    return "[No Name]"
  end
  return buf_name
end

local get_normal_wins = function(tab)
  return vim.tbl_filter(
    function(win) return vim.api.nvim_win_get_config(win).relative == "" end,
    vim.api.nvim_tabpage_list_wins(tab)
  )
end

local tab_active_p = function(tab) return tab == vim.api.nvim_get_current_tabpage() end

local tabline = function()
  local tabline = ""
  local tabs = vim.api.nvim_list_tabpages()
  for i, tab in ipairs(tabs) do
    local wins_visible = get_normal_wins(tab)
    local count_tab_wins = #wins_visible
    local win_current = tab_active_p(tab) and vim.api.nvim_get_current_win()
      or last_active_win[tab]
      or wins_visible[1]
    local buf_active = vim.api.nvim_win_get_buf(win_current or wins_visible[1])
    local name_buf_active = get_buf_name(buf_active)
    local buf_modified = vim.bo[buf_active].modified
    local hl_left = tab_active_p(tab) and "%#GRIMActiveLeft#" or "%#GRIMInactiveLeft#"
    local hl_text = tab_active_p(tab) and "%#GRIMActiveText#" or "%#GRIMInactiveText#"
    local hl_right = tab_active_p(tab) and "%#GRIMActiveRight#" or "%#GRIMInactiveRight#"
    local hl_icon = "GRIMIcon" .. i
    local icon, color = devicons.get_icon_color(
      name_buf_active,
      vim.fn.fnamemodify(name_buf_active, ":e"),
      { default = true }
    )
    vim.api.nvim_set_hl(0, hl_icon, { fg = color, bg = "NONE" })
    tabline = tabline .. hl_left .. " "
    tabline = tabline
      .. hl_text
      .. i
      .. "."
      .. " "
      .. "%#"
      .. hl_icon
      .. "#"
      .. icon
      .. hl_text
      .. " "
      .. name_buf_active
      .. " "
      .. (count_tab_wins > 1 and "[" .. count_tab_wins .. "] " or "")
      .. (buf_modified and S.Icons.ui.dot_l or " ")
      .. " "
      .. "|"
    tabline = tabline .. hl_right .. ""
    tabline = tabline .. "%#TabLine#"
  end
  return tabline
end

local highlight = function()
  local rp = require("clrs.road").palette
  local hl = vim.api.nvim_set_hl
  hl(0, "TabLine", { fg = "#666666", bg = "NONE" })
  hl(0, "TabLineFill", { bg = "NONE" })
  hl(0, "GRIMActiveLeft", { fg = "NONE", bg = "NONE" })
  hl(0, "GRIMActiveText", { fg = rp.lotusYellow["DEFAULT"], bg = "NONE" })
  hl(0, "GRIMActiveRight", { fg = "NONE", bg = "NONE" })
  hl(0, "GRIMInactiveLeft", { fg = "NONE", bg = "NONE" })
  hl(0, "GRIMInactiveText", { fg = rp.charcoal[700], bg = "NONE" })
  hl(0, "GRIMInactiveRight", { fg = "NONE", bg = "NONE" })
end

GRIM.tab = {
  init = function()
    highlight()
    vim.o.tabline = "%!v:lua.GRIM.tab.line()"
  end,
  line = tabline,
}
