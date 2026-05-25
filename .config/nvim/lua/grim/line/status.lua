-- Type --------------------------------------------------------------------------------------------

---GRIM.line.status.config is the default GRIM status line configuration.
---@class GRIM.line.status.config
---@field file { lalign: boolean, padding: { head: integer, tail: integer } }
---@field padding { head: integer, tail: integer }
---@field symbols {
---sep: string,
---buffer: { modified: string, readonly: string },
---git: { branch: string, add: string, change: string, del: string },
---}

---GRIM.line.status provides the GRIM status line.
---@class GRIM.line.status
---@field init fun(opts: GRIM.line.status.config): string
---@field component table

-- Config ------------------------------------------------------------------------------------------

---@type GRIM.line.status.config # Default GRIM status line configuration
local config = {
  file = { lalign = true, padding = { head = 1, tail = 1 } },
  padding = { head = 14, tail = 4 },
  symbols = {
    sep = GRIM.static.icon.ui.separator_statusline,
    buffer = { modified = GRIM.static.icon.ui.dot, readonly = GRIM.static.icon.ui.lock },
    git = {
      branch = GRIM.static.icon.git.branch,
      add = GRIM.static.icon.git.add_simple,
      change = GRIM.static.icon.git.change_simple_up,
      del = GRIM.static.icon.git.del_simple,
    },
  },
}

local component = {
  file = {
    ---@param bufid integer
    ---@return string
    name = function(bufid)
      return vim.bo[bufid].filetype == "oil" and string.format("Oil: %s", vim.fn.getcwd()) or "%t"
    end,
    ---@param path string
    ---@return string # File extension
    extension = function(path) return vim.fn.fnamemodify(path, ":e") end,
    path = { absolute = function() return "%F" end, relative = function() return "%f" end },
  },
  buffer = {
    number = function() return "%n" end,
    ---@param bufid integer
    ---@return string
    modified = function(bufid)
      return vim.api.nvim_get_option_value("modified", { buf = bufid })
          and config.symbols.buffer.modified
        or ""
    end,
    ---@param bufid integer
    ---@return string
    readonly = function(bufid)
      return vim.api.nvim_get_option_value("readonly", { buf = bufid })
          and config.symbols.buffer.readonly
        or ""
    end,
    help = function() return "%h" end,
    qlist = function() return "%q" end,
    search = function()
      if vim.v.hlsearch == 0 then return "" end
      local info = vim.fn.searchcount({ maxcount = 0 })
      return string.format("[%d/%d]", info.current, info.total)
    end,
  },
  macro = function()
    return vim.fn.reg_recording() ~= "" and "recording @" .. vim.fn.reg_recording() .. " " or ""
  end,
  ---@param bufid integer
  ---@return string # GRIM git diff status and branch name.
  git = function(bufid)
    if GRIM.git.versioned_p(bufid) then
      local status = GRIM.git.diffstat(bufid)
      return table.concat({
        status.added > 0 and config.symbols.git.add .. status.added or "",
        status.changed > 0 and config.symbols.git.change .. status.changed or "",
        status.removed > 0 and config.symbols.git.del .. status.removed or "",
        config.symbols.git.branch,
        GRIM.git.branch(bufid),
      }, " ")
    end
    return ""
  end,
  -- gitsigns = function(bufid)
  --   if GRIM.git.versioned_p(bufid) then
  --     local g = vim.b.gitsigns_status_dict
  --     if not g then return "" end
  --     -- { added = g.added, modified = g.changed, removed = g.removed }
  --     -- added = { config.symbols.git.add, fg = rp.emerald[300] }
  --     -- modified = { confing.symbols.git.change_simple_up, fg = rp.lotusYellow[200] }
  --     -- removed = { config.symbols.git.del, fg = rb.rustyRed }
  --     return table.concat({
  --     })
  --   end
  -- end,
  ---Padding.
  ---@param char string
  ---@param size integer
  pad = function(char, size) return size > 0 and string.rep(char, size) or char end,
  ---Groupping.
  ---@param components string[]
  ---@param joint string
  ---@param ljustify boolean
  ---@param width_min integer
  ---@param width_max integer
  ---@return string
  group = function(components, joint, ljustify, width_min, width_max)
    if width_min > 50 then width_min = 50 end
    return string.format(
      table.concat({
        "%%",
        ljustify and "-" or "",
        ((width_min ~= 0 and width_max ~= 0) and width_min .. "." .. width_max)
          or ((width_min ~= 0 and width_max == 0) and width_min)
          or ((width_min == 0 and width_max ~= 0) and "." .. width_max)
          or "",
        "(%s%%)",
      }, ""),
      table.concat(components, joint)
    )
  end,
  align = "%=",
  trunc = "%<",
}

-- Status line -------------------------------------------------------------------------------------

---@type GRIM.line.status
return {
  ---@param opts GRIM.line.status.config
  ---@return string # GRIM status line format string
  init = function(opts)
    config = vim.tbl_deep_extend("force", config, opts)
    local mod = {
      file = component.group({
        component.file.name(vim.api.nvim_get_current_buf()),
        component.buffer.modified(vim.api.nvim_get_current_buf()),
        component.buffer.search(),
        component.buffer.readonly(vim.api.nvim_get_current_buf()),
        component.buffer.help(),
        component.buffer.qlist(),
      }, " ", config.file.lalign, 32, 0),
      macro = component.group({ component.macro() }, "", true, 16, 0),
      git = component.group({ component.git(vim.api.nvim_get_current_buf()) }, "", false, 24, 0),
      diagnostics = component.group({
        next(vim.diagnostic.count(vim.api.nvim_get_current_buf()))
            and (vim.diagnostic.status() .. " " .. config.symbols.sep)
          or "",
      }, " ", false, 24, 0),
      head = table.concat({ component.pad(" ", config.padding.head), config.symbols.sep, " " }),
      tail = component.pad(" ", config.padding.tail),
    }
    return table.concat({
      mod.head,
      mod.file,
      mod.macro,
      component.trunc,
      component.align,
      mod.diagnostics,
      mod.git,
      mod.tail,
    })
  end,
  component = component,
}
