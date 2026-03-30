---@type vim.lsp.Config
return {
  name = "TexLab LS",
  cmd = { "texlab" },
  filetypes = { "tex", "plaintex", "bib" },
  root_markers = {
    "*.tex",
    "*.bib",
    ".latexmkrc",
    "latexmkrc",
    ".texlabroot",
    "texlabroot",
    "Tectonic.toml",
  },
  settings = {
    texlab = {
      rootDirectory = nil,
      build = {
        executable = "latexmk",
        args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
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
