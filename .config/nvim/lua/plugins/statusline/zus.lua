-- #  Zus - Statusline

Zus = {}

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Imports

require("config.functions")
local palette = require("nightfox.palette").load("nightfox")
local Color = require('nightfox.lib.color')

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Highlight, color, assets & icons

Zus.HIGHLIGHT= {
    ZusHL               = "%#ZusHL#",
    ZusHLMode           = "%#ZusHLMode#",
    ZusHLNormal         = "%#ZusHLNormal#",
    ZusHLVisual         = "%#ZusHLVisual#",
    ZusHLInsert         = "%#ZusHLInsert#",
    ZusHLCommand        = "%#ZusHLCommand#",
    ZusHLSelect         = "%#ZusHLVisual#",
    ZusHLReplace        = "%#ZusHLVisual#",
    ZusHLTerminal       = "%#ZusHLVisual#",
    ZusLSPError         = "%#ZusLSPError#",
    ZusLSPWarn          = "%#ZusLSPWarn#",
    ZusLSPHint          = "%#ZusLSPHint#",
    ZusLSPInfo          = "%#ZusLSPInfo#",
}

Zus.diagnostic_icons = {
    error = "  ",
    warn  = "  ",
    hint  = "  ",
    info  = "  "
}

local statusline_bg = Color.from_hex(palette.bg0)
local statusline_fg = Color.from_hex(palette.fg2)

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Helper functions

local get_diagnostic_stats = function()
    local error_count, warn_count, info_count, hint_count
    local diagnostics = vim.diagnostic.get(0)
    local count = { 0, 0, 0, 0 }
    for _, diagnostic in ipairs(diagnostics) do
        if vim.startswith(vim.diagnostic.get_namespace(diagnostic.namespace).name, 'vim.lsp') then
            count[diagnostic.severity] = count[diagnostic.severity] + 1
        end
    end
    error_count = count[vim.diagnostic.severity.ERROR]
    warn_count = count[vim.diagnostic.severity.WARN]
    info_count = count[vim.diagnostic.severity.INFO]
    hint_count = count[vim.diagnostic.severity.HINT]
    return error_count, warn_count, info_count, hint_count
end

Diagnostics = function()
    local signs = { error = "  ", warn = "  ", hint = "  ", info = "  " }
    local error, warn, info, hint = get_diagnostic_stats()
    local result = (error > 0 and signs.error .. error or "")
        .. (warn > 0 and signs.warn .. warn or "")
        .. (hint > 0 and signs.hint .. hint or "")
        .. (info > 0 and signs.info .. info or "")

    return result
end

Git_branch = function()
    local branch_name = vim.fn.FugitiveStatusline()
    local clean_name = string.match(branch_name, "%(([a-zA-Z0-9]+)%)")
    local git_icon = " "

    if clean_name == nil then
        return ""
    end

    return git_icon .. clean_name
end

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Components

Zus.components = {}
local zc = Zus.components

zc.start_spacing        = "%-2.(| %)"
zc.end_spacing          = "%2.( |%)"
zc.mode                 = "%16.(%{%v:lua.F.NvimMode()%}%)"
zc.file_name            = "%-24.(%t%)"
zc.file_status          = "%(%-3.m - %-4.r%)"
zc.git_status           = "%10.(%{get(b:,'gitsigns_status','')}%)"
zc.git_branch           = "%8.(%{%v:lua.Git_branch()%}%)"
zc.lsp_diagnostics      = "%16.(%{%v:lua.Diagnostics()%}%)"

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Sections

Zus.sections = {}
local zs = Zus.sections

Zus.sections.left  = zc.start_spacing .. zc.file_name .. "%<"
Zus.sections.mid   = "%=" .. zc.mode .. "%="
Zus.sections.right = zc.git_status .. " " .. zc.git_branch .. zc.lsp_diagnostics .. zc.end_spacing

-- ~ -------------------------------------------------------------------------------- ~ --

vim.opt.statusline = string.format("%s%s%s%s",
    Zus.HIGHLIGHT.ZusHL,
    zs.left,
    zs.mid,
    zs.right
)

vim.cmd([[hi ZusHL guifg=statusline_fg guibg=statusline_bg]])

return Zus
