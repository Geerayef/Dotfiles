-- #  Zus - Tabline

ZusT = {}

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Imports

require("config.functions")

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Options

ZusT.options = {
    show_name = true,
    show_index = true,
    show_modified_status = true,
    show_icon = false,
    fnamemodify = ":t",
    brackets = { "[", "]" },
    unnamed_buffer_label = "",
    symbol_modified = " [+]",
    inactive_tab_max_length = 0,
}

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Helper functions

local fn = vim.fn

local function tabline(options)
    local tab = ""
    for index = 1, fn.tabpagenr("$") do
        local winnr = fn.tabpagewinnr(index)
        local buflist = fn.tabpagebuflist(index)
        local bufnr = buflist[winnr]
        local bufname = fn.bufname(bufnr)
        local bufmodified = fn.getbufvar(bufnr, "&mod")

        tab = tab .. "%" .. index .. "T"
        if index == fn.tabpagenr() then
            tab = tab .. "%#TabLineSel#"
        else
            tab = tab .. "%#ZusHL#"
        end

        tab = tab .. " "

        if options.show_index then
            tab = tab .. index .. "."
        end

        local icon = ""
        if options.show_icon and ZusT.has_devicons then
            local ext = fn.fnamemodify(bufname, ':e')
            icon = ZusT.devicons.get_icon(bufname, ext, { default = true }) .. " "
        end

        if options.show_name then
            tab = tab .. options.brackets[1]
            local pre_title_s_len = string.len(tab)
            if bufname ~= "" then
                tab = tab .. icon .. fn.fnamemodify(bufname, options.fnamemodify)
            else
                tab = tab .. options.unnamed_buffer_label
            end

            if
                options.inactive_tab_max_length
                and options.inactive_tab_max_length > 0
                and index ~= fn.tabpagenr()
            then
                tab = string.sub(
                    tab,
                    1,
                    pre_title_s_len + options.inactive_tab_max_length
                )
            end
            tab = tab .. options.brackets[2]
        end

        if
            bufmodified == 1
            and options.show_modified_status
            and options.symbol_modified ~= nil
        then
            tab = tab .. options.symbol_modified
        end

        tab = tab .. " "
    end

    tab = tab .. "%#ZusHL#"
    return tab
end

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Setup

function ZusT.setup(user_options)
    ZusT.options = vim.tbl_extend("force", ZusT.options, user_options)
    ZusT.has_devicons, ZusT.devicons = pcall(require, "nvim-web-devicons")

    function ZusTabline()
        return tabline(ZusT.options)
    end

    vim.o.tabline = "%!v:lua.ZusTabline()"
end

return ZusT
