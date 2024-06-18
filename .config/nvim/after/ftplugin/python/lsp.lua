local lsp = require("util.lsp")
local ruff_lsp = {
  filetypes = { "python" },
  root_patterns = { "*.py" },
  cmd = { "ruff-lsp" },
}
local pylsp = {
  filetypes = { "python" },
  root_patterns = { "*.py" },
  settings = {
    pylsp = {
      plugins = {
        autopep8 = { enabled = false },
        flake8 = { enabled = false },
        yapf = { enabled = false },
        mccabe = { enabled = false },
        pycodestyle = { enabled = false },
        pyflakes = { enabled = false },
        pylint = { enabled = false },
      },
    },
  },
  cmd = { "pylsp" },
}

lsp.start(ruff_lsp)
lsp.start(pylsp)
