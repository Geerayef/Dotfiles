vim.lsp.handlers["textDocument/hover"] =
  vim.lsp.with(vim.lsp.handlers.hover, { border = S.Border })
vim.lsp.handlers["textDocument/signatureHelp"] =
  vim.lsp.with(vim.lsp.handlers.signature_help, { border = S.Border })

vim.diagnostic.config({
  underline = true,
  severity_sort = true,
  update_in_insert = false,
  signs = true,
  virtual_text = {
    spacing = 4,
    severity = nil,
    source = "if_many",
    format = nil,
    prefix = S.Icons.ui.dot_xl,
  },
  float = {
    source = "if_many",
    wrap_at = 64,
    format = function(d)
      if not d.code and not d.user_data then return d.message end
      local t = vim.deepcopy(d)
      local code = d.code
      if not code then
        if not d.user_data.lsp then return d.message end
        code = d.user_data.lsp.code
      end
      if code then
        t.message = string.format("%s [%s]", t.message, code):gsub("1. ", "")
      end
      return t.message
    end,
    border = S.BorderSimple,
  },
})
