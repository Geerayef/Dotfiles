local ruff = {
  on_attach = require("core.func").LSPAttach,
  cmd = { "ruff", "server" },
  filetypes = { "python" },
  root_patterns = { "*.py" },
  settings = {
    lint = { preview = true },
  },
}

local basedpyright = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "python" },
  root_patterns = { "*.py" },
  settings = {
    python = {
      analysis = {
        autoSearchPaths = true,
        diagnosticMode = "openFilesOnly",
        useLibraryCodeForTypes = true,
      },
    },
  },
  cmd = { "basedpyright-langserver", "--stdio" },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(ruff) end
  )
end)

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(basedpyright) end
  )
end)
