local last_active_win = {}
local devicons = require("nvim-web-devicons")
local rb = require("clrs.road").base
local rp = require("clrs.road").palette

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

local highlight = function()
  local hl = vim.api.nvim_set_hl
  hl(0, "TabLine", { fg = rp.charcoal[700], bg = rb.dragonInk })
  hl(0, "TabLineFill", { bg = rb.dragonInk })
  hl(0, "GRIMActive", { bg = rb.dragonInk })
  hl(0, "GRIMInactive", { fg = rp.charcoal[700], bg = rb.dragonInk })
  hl(0, "GRIMActiveText", { fg = rb.lotusYellow, bg = rb.dragonInk })
  hl(0, "GRIMInactiveText", { fg = rp.charcoal[700], bg = rb.dragonInk })
end

local tabline = function()
  local format = ""
  local hlgrp = {
    GRIMActive = "%#GRIMActive#",
    GRIMInactive = "%#GRIMInactive#",
    GRIMActiveText = "%#GRIMActiveText#",
    GRIMInactiveText = "%#GRIMInactiveText#",
  }
  local tabs = vim.api.nvim_list_tabpages()
  for i, tab in ipairs(tabs) do
    local tab_active = tab_active_p(tab)
    local wins_visible = get_normal_wins(tab)
    local count_tab_wins = #wins_visible
    local win_current = tab_active and vim.api.nvim_get_current_win()
      or last_active_win[tab]
      or wins_visible[1]
    local buf_active = vim.api.nvim_win_get_buf(win_current or wins_visible[1])
    local name_buf_active = get_buf_name(buf_active)
    local buf_modified = vim.bo[buf_active].modified
    local hl_text = tab_active and hlgrp.GRIMActiveText or hlgrp.GRIMInactiveText
    local hl_def = tab_active and hlgrp.GRIMActive or hlgrp.GRIMInactive
    local hl_icon = "GRIMIcon" .. i
    local icon, color = devicons.get_icon_color(
      name_buf_active,
      vim.fn.fnamemodify(name_buf_active, ":e"),
      { default = true }
    )
    vim.api.nvim_set_hl(0, hl_icon, { fg = color, bg = rb.dragonInk })
    format = format .. hl_def .. " "
    format = format
      .. hl_text
      .. i
      .. ". "
      .. "%#"
      .. hl_icon
      .. "#"
      .. icon
      .. hl_text
      .. " "
      .. name_buf_active
      .. " "
      .. (count_tab_wins > 1 and "[" .. count_tab_wins .. "] " or "")
      .. (buf_modified and GRIM.static.icon.ui.dot_l or " ")
      .. " |"
    format = format .. hl_def .. "%#TabLine#"
  end
  format = format .. "%#TabLineFill#"
  highlight()
  return format
end

---GRIM.tab provies the tabline.
---@class GRIM.tab
---@field line fun(): string -- # GRIM tab line definition
return { line = tabline }
