vim.diagnostic.config({
  virtual_text = true,
  virtual_lines = false,
  severity_sort = true,
  jump = {
    on_jump = function(diagnostic, bufid)
      if not diagnostic then return end
      vim.diagnostic.open_float({ bufnr = bufid, scope = "cursor" })
    end,
  },
  float = {
    scope = "cursor",
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
    border = GRIM.static.border_simple,
  },
  signs = {
    text = {
      [vim.diagnostic.severity.HINT] = GRIM.static.icon.diagnostics.hint_ascii,
      [vim.diagnostic.severity.INFO] = GRIM.static.icon.diagnostics.info_ascii,
      [vim.diagnostic.severity.WARN] = GRIM.static.icon.diagnostics.warn_ascii,
      [vim.diagnostic.severity.ERROR] = GRIM.static.icon.diagnostics.error_ascii,
    },
    linehl = { [vim.diagnostic.severity.ERROR] = "ErrorMsg" },
    numhl = { [vim.diagnostic.severity.WARN] = "WarningMsg" },
  },
  status = {
    format = {
      [vim.diagnostic.severity.HINT] = GRIM.static.icon.diagnostics.hint_empty,
      [vim.diagnostic.severity.INFO] = GRIM.static.icon.diagnostics.info_empty,
      [vim.diagnostic.severity.WARN] = GRIM.static.icon.diagnostics.warn_empty,
      [vim.diagnostic.severity.ERROR] = GRIM.static.icon.diagnostics.error_empty,
    },
  },
})
