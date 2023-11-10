-- ~  Map leader

vim.g.mapleader = " "

-- ---------------------------------------- --

-- ~  lazy

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
vim.opt.rtp:prepend(lazypath)
local status, lazy = pcall(require, "lazy")
if not status then return end

local small_lazy_options = {
  root = vim.fn.stdpath("data") .. "/lazy",
  defaults = { lazy = false, version = nil, cond = nil, },
  spec = nil,
  performance = { cache = { enabled = true, },
    reset_packpath = true,
    rtp = { reset = false, disabled_plugins = { "gzip",
      "matchit", "matchparen", "netrwPlugin", "tarPlugin",
      "tohtml", "tutor", "zipPlugin", },
    },
  },
}

lazy.setup({ import = "plugins.colorscheme" }, small_lazy_options)

-- ---------------------------------------- --

-- ~  Functions

NvimMode = function()
  local n = 'normal'
  local v = 'visual'
  local i = 'insert'
  local c = 'command'
  local r = 'replace'
  local s = 'select'
  local t = 'terminal'
  local modes = {
    ['n']     = n,
    ['no']    = n,
    ['nov']   = n,
    ['noV']   = n,
    ['no\22'] = n,
    ['niI']   = n,
    ['niR']   = n,
    ['niV']   = n,
    ['nt']    = n,
    ['ntT']   = n,
    ['v']     = v,
    ['vs']    = v,
    ['V']     = v,
    ['Vs']    = v,
    ['\22']   = v,
    ['\22s']  = v,
    ['s']     = s,
    ['S']     = s,
    ['\19']   = i,
    ['i']     = i,
    ['ic']    = i,
    ['ix']    = i,
    ['R']     = r,
    ['Rc']    = r,
    ['Rx']    = r,
    ['Rv']    = r,
    ['Rvc']   = r,
    ['Rvx']   = r,
    ['c']     = c,
    ['cv']    = c,
    ['ce']    = c,
    ['r']     = r,
    ['!']     = '󰩌',
    ['t']     = t,
  }
  return modes[vim.api.nvim_get_mode().mode] or '[Unknown]'
end

-- ---------------------------------------- --

-- ~  Statusline

local minimaline = {}
minimaline.components = {}
local mc = minimaline.components
minimaline.sections = {}
local ms = minimaline.sections
minimaline.highlight = "%#MiniHL#"

-- Components
mc.start_spacing = "| %<"
mc.end_spacing   = " |"
mc.mode          = "%8.{%v:lua.NvimMode()%}"
mc.file_name     = "%-24.t"
mc.file_status   = "%-8.(%-3.m %-4.r%)"

-- Sections
ms.left  = "%(" .. mc.start_spacing .. mc.file_name .. mc.file_status .. "%)"
ms.mid   = "%=%(" .. mc.mode .. "%)"
ms.right = "%=%(" .. "%)" .. mc.end_spacing

-- ---------------------------------------- --

-- ~  Options

local o = vim.opt
local cmd = vim.cmd

o.shell = "/usr/bin/fish"
o.compatible = false

--  File
cmd.syntax("on")
cmd.filetype("plugin", "indent", "on")
o.fileformats = "unix"

--  Edit
o.virtualedit = "block"
o.expandtab = true

--  UI
o.background = "dark"
o.guicursor = ""
o.cursorline = true
o.cursorlineopt = "number"
o.termguicolors = true
o.number = true
o.statusline = string.format("%s%s%s%s",
    minimaline.highlight,
    ms.left,
    ms.mid,
    ms.right
)
o.laststatus = 3
o.showtabline = 1
o.title = true
o.showcmd = false
o.showmode = false
o.showmatch = true
o.cmdheight = 1
o.ruler = false
o.wildmenu = true

--  Behaviour
o.ttyfast = true
o.lazyredraw = true
o.updatetime = 300
o.belloff = "all"
o.splitright = true
o.splitbelow = true
o.scrolloff = 10
o.sidescrolloff = 10

--  Search
o.grepprg = "rg"
o.ignorecase = true
o.smartcase = true
o.hlsearch = true
o.incsearch = true

--  Color
vim.cmd("colorscheme midnight")
vim.cmd("highlight Comment gui=none")
vim.cmd("highlight link MiniHL Normal")
