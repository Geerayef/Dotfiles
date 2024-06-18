local lsp = require("util.lsp")
local texlab = {
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
      formatterLineLength = 96,
    },
  },
  cmd = { "texlab" },
}

if lsp.start(texlab) ~= nil then
  F.Notify("INFO", texlab.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. texlab.cmd[1])
end
