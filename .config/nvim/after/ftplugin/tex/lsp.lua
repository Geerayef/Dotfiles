local texlab = {
  name = "TexLab LS",
  cmd = { "texlab" },
  on_attach = LSP.attach,
  filetypes = { "tex", "plaintex", "bib" },
  root_markers = { "*.tex", "*.bib", ".latexmkrc" },
  settings = {
    texlab = {
      rootDirectory = nil,
      build = {
        executable = "latexmk",
        args = {
          "-pdf",
          "-interaction=nonstopmode",
          "-synctex=1",
          "%f",
        },
        onSave = false,
        forwardSearchAfter = false,
      },
      auxDirectory = ".",
      forwardSearch = { executable = nil, args = {} },
      chktex = { onOpenAndSave = false, onEdit = false },
      diagnosticsDelay = 300,
      latexFormatter = "latexindent",
      latexindent = { ["local"] = nil, modifyLineBreaks = false },
      bibtexFormatter = "texlab",
      formatterLineLength = 80,
    },
  },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(texlab) end
  )
end)
