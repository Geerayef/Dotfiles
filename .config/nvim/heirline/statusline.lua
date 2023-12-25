local StatusLine = {}

local Icon = require("nvim-web-devicons")
local Util = require("heirline.utils")
local Cond = require("heirline.conditions")

-- ~ -------------------------------------------------------------------------------- ~ --

local set_offset = function (offset, component) return "%" .. offset .. ".(" .. component .. "%)" end

local offset = {
  filestatus_group = -7,
  fileflag = 3,
  filename = -24,
  macro = -3,
  icon = -2,
  mode = 8,
  git_branch = 8,
  git_sign = 4,
  git_status = 12,
  diagnostic = 22,
  diag_sign = 5,
}

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  File

local FileNameBlock = { init = function (self) self.filename = vim.api.nvim_buf_get_name(0) end }

local FileIcon = {
  init = function (self)
    local filename = self.filename
    local extension = vim.fn.fnamemodify(filename, ":e")
    self.icon, self.icon_color = Icon.get_icon_color(filename, extension, { default = true })
  end,
  provider = function (self) return self.icon and set_offset(offset.icon, self.icon .. " ") end,
  hl = function (self) return { fg = self.icon_color } end
}

local FileName = {
  provider = function (self)
    local filename = vim.fn.fnamemodify(self.filename, ":t")
    if filename == "" then return set_offset(offset.filename, "[No name]") end
    if not Cond.width_percent_below(#filename, 0.25) then
      filename = vim.fn.pathshorten(filename)
    end
    return set_offset(offset.filename, filename)
  end,
  hl = { fg = Util.get_highlight("Directory").fg }
}

local FileFlags = {
  { provider = "%" .. offset.filestatus_group .. ".(" },
  {
    condition = function() return vim.bo.modified end,
    provider = set_offset(offset.fileflag,"[+]"),
    hl = { fg = "green" }
  },
  { provider = " " },
  {
    condition = function() return not vim.bo.modifiable or vim.bo.readonly end,
    provider = set_offset(offset.fileflag, ""),
    hl = { fg = "orange" }
  },
  { provider = "%)" },
}

FileNameBlock = Util.insert(FileNameBlock, FileIcon, FileName, FileFlags, { provider = "%<" })

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Mode

local Mode = {
  init = function (self) self.mode = vim.fn.mode(1) end,
  static = {
    modes = {
      ["n"]     = "normal",
      ["no"]    = "normal",
      ["nov"]   = "normal",
      ["noV"]   = "normal",
      ["no\22"] = "normal",
      ["niI"]   = "normal",
      ["niR"]   = "normal",
      ["niV"]   = "normal",
      ["nt"]    = "normal",
      ["ntT"]   = "normal",
      ["v"]     = "visual",
      ["vs"]    = "visual",
      ["V"]     = "visual",
      ["Vs"]    = "visual",
      ["\22"]   = "visual",
      ["\22s"]  = "visual",
      ["s"]     = "select",
      ["S"]     = "select",
      ["\19"]   = "insert",
      ["i"]     = "insert",
      ["ic"]    = "insert",
      ["ix"]    = "insert",
      ["R"]     = "replace",
      ["Rc"]    = "replace",
      ["Rx"]    = "replace",
      ["Rv"]    = "replace",
      ["Rvc"]   = "replace",
      ["Rvx"]   = "replace",
      ["c"]     = "command",
      ["cv"]    = "command",
      ["ce"]    = "command",
      ["r"]     = "...",
      ["rm"]    = "M",
      ["r?"]    = "?",
      ["!"]     = "󰩌",
      ["t"]     = "terminal",
    },
    mode_colors = {
      n       = "gray",
      ["!"]   = "red",
      t       = "red",
      i       = "green",
      v       = "cyan",
      V       = "cyan",
      ["\22"] = "cyan",
      c       = "orange",
      r       = "orange",
      R       = "orange",
      s       = "purple",
      S       = "purple",
      ["\19"] = "purple",
    }
  },
  provider = function (self) return set_offset(offset.mode, self.modes[self.mode]) end,
  hl = function (self)
    local mode = self.mode:sub(1, 1)
    return { fg = self.mode_colors[mode], bold = true }
  end,
  update = { "ModeChanged", pattern = "*:*",
    callback = vim.schedule_wrap(function () vim.cmd.redrawstatus() end)
  }
}

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Git

local Git = {
  condition = Cond.is_git_repo,
  init = function(self)
    self.status_dict = vim.b.gitsigns_status_dict
    self.has_changes = self.status_dict.added ~= 0 or self.status_dict.removed ~= 0 or self.status_dict.changed ~= 0
  end,
  hl = { fg = "orange" },
  {
    provider = function(self) return set_offset(offset.git_branch, " " .. self.status_dict.head) end,
    hl = { bold = true }
  },
  { condition = function(self) return self.has_changes end },
  { provider = "%" .. offset.git_status .. ".(" },
  {
    provider = function(self)
      local added = self.status_dict.added or 0
      return added > 0 and set_offset(offset.git_sign, "+" .. added)
    end,
    hl = { fg = "git_add" }
  },
  {
    provider = function(self)
      local deleted = self.status_dict.removed or 0
      return deleted > 0 and set_offset(offset.git_sign, "-" .. deleted)
    end,
    hl = { fg = "git_del" }
  },
  {
    provider = function(self)
      local changed = self.status_dict.changed or 0
      return changed > 0 and set_offset(offset.git_sign, "~" .. changed)
    end,
    hl = { fg = "git_change" }
  },
  { provider = "%)" },
}

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Diagnostics

local Diagnostics = {
  condition = Cond.has_diagnostics,
  static = { error_icon = "  ", warn_icon  = "  ", info_icon  = "  ", hint_icon  = "  " },
  init = function(self)
    self.error = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
    self.warn = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
    self.hint = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
    self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
  end,
  update = { "DiagnosticChanged", "BufEnter" },
  { provider = "%" .. offset.diagnostic .. ".([" },
  {
    provider = function(self)
      return self.error > 0 and set_offset(offset.diag_sign, self.error_icon .. self.error .. " ")
    end,
    hl = { fg = "diag_error" },
  },
  {
    provider = function(self)
      return self.warn > 0 and set_offset(offset.diag_sign, self.warn_icon .. self.warn .. " ")
    end,
    hl = { fg = "diag_warn" },
  },
  {
    provider = function(self)
      return self.info > 0 and set_offset(offset.diag_sign, self.info_icon .. self.info .. " ")
    end,
    hl = { fg = "diag_info" },
  },
  {
    provider = function(self)
      return self.hint > 0 and set_offset(offset.diag_sign, self.hint_icon .. self.hint .. " ")
    end,
    hl = { fg = "diag_hint" },
  },
  { provider = "]%)" },
}

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Small utilities

-- ~  Help file name

local HelpFileName = {
  condition = function() return vim.bo.filetype == "help" end,
  provider = function()
    local filename = vim.api.nvim_buf_get_name(0)
    return set_offset(offset.filename, vim.fn.fnamemodify(filename, ":t"))
  end,
  hl = { fg = "blue" },
}

-- ~  File type

local FileType = {
    provider = function() return set_offset(offset.filename, vim.bo.filetype) end,
    hl = { fg = Util.get_highlight("Type").fg, bold = true },
}

-- ~  Macro

local MacroRec = {
  condition = function() return vim.fn.reg_recording() ~= "" and vim.o.cmdheight == 0 end,
  provider = " ",
  hl = { fg = "orange", bold = true },
  { provider = function() return vim.fn.reg_recording() end, hl = { fg = "green", bold = true } },
  update = { "RecordingEnter", "RecordingLeave" }
}

-- ~  Ruler

local Ruler = { provider = "%4L" }

-- ~  Commands

local ShowCmd = { condition = function () return vim.o.cmdheight == 0 end, provider = ":%3.5(%S%)" }

local Align = { provider = "%=" }
local Space = { provider = " " }
local Start = { provider = "| " }
local End = { provider = " |" }

-- ~  --------------------------------------------------------------------------------  ~ --

-- ~  Assemble statuslines

local Default = { Start, FileNameBlock, Align, Mode, Align, Git, Diagnostics, End }

local Inactive = { condition = Cond.is_not_active, FileType, Space, FileName, ShowCmd, Align }

local Special = {
  condition = function()
    return Cond.buffer_matches({
      buftype = { "nofile", "prompt", "help", "quickfix" }, filetype = { "^git.*", "fugitive" }
    })
  end,
  FileType, Space, HelpFileName, ShowCmd, Ruler, Align
}

StatusLine = {
  hl = function ()
    if Cond.is_active() then return "StatusLine" else return "StatusLineNC" end
  end,
  fallthrough = false,
  Special, Inactive, Default
}

return StatusLine
