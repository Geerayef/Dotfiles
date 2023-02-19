local status indent_blankline = pcall(require, "indent-blankline")
if not status then
    return
end

indent_blankline.setup {
  char = '┊',
  show_trailing_blankline_indent = false,
}
