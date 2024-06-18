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

if lsp.start(pylsp) ~= nil then
  F.Notify("INFO", pylsp.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. pylsp.cmd[1])
end

if lsp.start(ruff_lsp) ~= nil then
  F.Notify("INFO", ruff_lsp.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. ruff_lsp.cmd[1])
end
