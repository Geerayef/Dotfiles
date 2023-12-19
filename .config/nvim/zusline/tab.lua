-- #  Zus tabline

ZusT = {}

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Imports


-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Options

ZusT.options = {
  show_icons = false,
  show_index = true,
  show_name = true,
  show_modified_status = true,
  fnamemodify = ":t",
  brackets = { "[", "]" },
  unnamed_buffer_label = "",
  symbol_modified = " [+]",
  inactive_tab_max_length = 0,
}

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Tabline string builder

local build_tabline = function (options)
  local fn = vim.fn
  local tab = ""

  for index = 1, fn.tabpagenr("$") do
    local winnr = fn.tabpagewinnr(index)
    local buflist = fn.tabpagebuflist(index)
    local bufnr = buflist[winnr]
    local bufname = fn.bufname(bufnr)
    local bufmodified = fn.getbufvar(bufnr, "&mod")
    local icon = ""
    tab = tab .. "%" .. index .. "T"

    if index == fn.tabpagenr() then
      tab = tab .. "%#TabLineSel#"
    else
      tab = tab .. Zus.HIGHLIGHT.ZusHL
    end

    tab = tab .. " "

    if options.show_index then tab = tab .. index .. "." end

    if options.show_icons and ZusT.has_devicons then
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

      if options.inactive_tab_max_length
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

    if bufmodified == 1
      and options.show_modified_status
      and options.symbol_modified ~= nil
    then
      tab = tab .. options.symbol_modified
    end

    tab = tab .. " "
  end
  return tab
end

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  Setup

ZusT.setup = function (user_options)
  ZusT.options = vim.tbl_extend("force", ZusT.options, user_options)
  ZusT.has_devicons, ZusT.devicons = pcall(require, "nvim-web-devicons")

  vim.opt.tabline = string.format("%s", build_tabline(user_options))
end

return ZusT
