local ruff = {
  name = "Ruff LS",
  cmd = { "ruff", "server" },
  on_attach = LSP.attach,
  filetypes = { "python" },
  root_markers = { "*.py" },
  settings = {
    lint = { preview = true },
  },
}

local basedpyright = {
  name = "BasedPyright LS",
  cmd = { "basedpyright-langserver", "--stdio" },
  on_attach = LSP.attach,
  filetypes = { "python" },
  root_markers = { "*.py" },
  settings = {
    python = {
      analysis = {
        autoSearchPaths = true,
        diagnosticMode = "openFilesOnly",
        useLibraryCodeForTypes = true,
      },
    },
  },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(ruff) end
  )
end)

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(basedpyright) end
  )
end)
