GRIM = {}

local function get_buf_name(buf)
  local buf_name = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":t")
  local buf_type = vim.bo[buf].buftype
  local buf_ft = vim.bo[buf].filetype
  if buf_type == "help" then
    return "Help: " .. buf_name
  elseif buf_ft == "oil" then
    return "Oil: " .. (buf_name ~= "" and buf_name or ".")
  elseif buf_type == "terminal" then
    return "Terminal"
  elseif buf_type == "quickfix" then
    return "Quickfix"
  elseif buf_name == "" then
    return "[No Name]"
  end
  return buf_name
end

local function get_normal_wins(tab)
  return vim.tbl_filter(
    function(win) return vim.api.nvim_win_get_config(win).relative == "" end,
    vim.api.nvim_tabpage_list_wins(tab)
  )
end

GRIM.Tabline = function()
  local tabline = ""
  local tabs = vim.api.nvim_list_tabpages()
  for i, tab in ipairs(tabs) do
    local wins_visible = get_normal_wins(tab)
    local tab_win_count = #wins_visible
    local is_active = (tab == vim.api.nvim_get_current_tabpage())
    local win_current = is_active and vim.api.nvim_get_current_win()
      or wins_visible[1]
    local buf_active = vim.api.nvim_win_get_buf(
      win_current or vim.api.nvim_tabpage_list_wins(tab)[1]
    )
    local buf_active_name = get_buf_name(buf_active)
    local modified = vim.tbl_contains(
      vim.tbl_map(
        function(win) return vim.bo[vim.api.nvim_win_get_buf(win)].modified end,
        wins_visible
      ),
      true
    )
    local hl_left = is_active and "%#GRIMActiveLeft#" or "%#GRIMInactiveLeft#"
    local hl_text = is_active and "%#GRIMActiveText#" or "%#GRIMInactiveText#"
    local hl_right = is_active and "%#GRIMActiveRight#"
      or "%#GRIMInactiveRight#"
    tabline = tabline .. hl_left .. " "
    tabline = tabline
      .. hl_text
      .. i
      .. "."
      .. " "
      .. buf_active_name
      .. " "
      .. (tab_win_count > 1 and "[" .. tab_win_count .. "] " or "")
      .. (modified and S.Icons.ui.dot or " ")
      .. " "
      .. "|"
    tabline = tabline .. hl_right .. ""
    tabline = tabline .. "%#TabLine# "
  end
  return tabline
end

GRIM.Highlight = function()
  local rp = require("clrs.road").palette
  vim.api.nvim_set_hl(0, "TabLine", { fg = "#666666", bg = "NONE" })
  vim.api.nvim_set_hl(0, "TabLineFill", { bg = "NONE" })
  vim.api.nvim_set_hl(0, "GRIMActiveLeft", { fg = "NONE", bg = "NONE" })
  vim.api.nvim_set_hl(0, "GRIMActiveText", { fg = rp.lotusYellow["DEFAULT"], bg = "NONE" })
  vim.api.nvim_set_hl(0, "GRIMActiveRight", { fg = "NONE", bg = "NONE" })
  vim.api.nvim_set_hl(0, "GRIMInactiveLeft", { fg = "NONE", bg = "NONE" })
  vim.api.nvim_set_hl(0, "GRIMInactiveText", { fg = rp.charcoal[600], bg = "NONE" })
  vim.api.nvim_set_hl(0, "GRIMInactiveRight", { fg = "NONE", bg = "NONE" })
end

GRIM.Init = function()
  GRIM.Highlight()
  vim.o.tabline = "%!v:lua.GRIM.Tabline()"
end

return GRIM
