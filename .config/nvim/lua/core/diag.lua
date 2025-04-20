vim.diagnostic.config({
  severity_sort = true,
  signs = true,
  underline = true,
  update_in_insert = false,
  virtual_text = false,
  virtual_lines = false,
  float = {
    scope = "line",
    source = "if_many",
    format = function(d)
      local code = d.code
      if not code and not d.user_data then return d.message end
      local t = vim.deepcopy(d)
      if not code then
        if not d.user_data.lsp then return d.message end
        code = d.user_data.lsp.code
      end
      if code then
        t.message = string.format("%s [%s]", t.message, code):gsub("1. ", "")
      end
      return t.message
    end,
    border = S.Border,
  },
})
