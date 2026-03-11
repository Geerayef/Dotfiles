vim.diagnostic.config({
  underline = true,
  virtual_text = true,
  virtual_lines = false,
  update_in_insert = false,
  severity_sort = true,
  signs = true,
  jump = { float = true },
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
      else
        t.message = string.format("%s [%s]", t.message, code):gsub("1. ", "")
      end
      return t.message
    end,
    border = S.BorderSimple,
  },
})
