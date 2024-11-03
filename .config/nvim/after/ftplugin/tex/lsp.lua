local texlab = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "tex", "plaintex", "bib" },
  root_patterns = { "*.tex", "*.bib", ".latexmkrc" },
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
  cmd = { "texlab" },
}

require("util.lsp").start(texlab)
