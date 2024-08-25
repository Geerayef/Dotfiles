local ruff = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "python" },
  root_patterns = { "*.py" },
  cmd = { "ruff", "server" },
}

local pylsp = {
  on_attach = require("core.func").LspAttach,
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

LSP.start(ruff)
LSP.start(pylsp)
