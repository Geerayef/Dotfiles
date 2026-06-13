local i = GRIM.static.icon
local severity = vim.diagnostic.severity
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
      [severity.HINT] = i.diagnostics.hint_ascii,
      [severity.INFO] = i.diagnostics.info_ascii,
      [severity.WARN] = i.diagnostics.warn_ascii,
      [severity.ERROR] = i.diagnostics.error_ascii,
    },
  },
  status = {
    format = function(counts)
      local d = {
        [severity.HINT] = { i.diagnostics.hint_empty, "DiagnosticHint" },
        [severity.INFO] = { i.diagnostics.info_empty, "DiagnosticInfo" },
        [severity.WARN] = { i.diagnostics.warn_empty, "DiagnosticWarn" },
        [severity.ERROR] = { i.diagnostics.error_empty, "DiagnosticError" },
      }
      return table.concat(
        vim
          .iter(vim.tbl_keys(counts))
          :map(function(s) return ("%%#%s#%s %s"):format(d[s][2], d[s][1], counts[s]) end)
          :totable(),
        " "
      )
    end,
  },
})
