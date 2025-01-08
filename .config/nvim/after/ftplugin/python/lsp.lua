local ruff = {
  on_attach = require("core.func").LSPAttach,
  cmd = { "ruff", "server" },
  filetypes = { "python" },
  root_patterns = { "*.py" },
  settings = {
    lint = { preview = true }
  }
}

local pylsp = {
  on_attach = require("core.func").LSPAttach,
  cmd = { "pylsp" },
  filetypes = { "python" },
  root_patterns = { "*.py" },
  settings = {
    pylsp = {
      plugins = {
        jedi_completion     = { enabled = true },
        jedi_definition     = { enabled = true },
        jedi_hover          = { enabled = true },
        jedi_signature_help = { enabled = true },
        jedi_references     = { enabled = true },
        jedi_symbols        = { enabled = true },
        autopep8    = { enabled = false },
        flake8      = { enabled = false },
        mccabe      = { enabled = false },
        preload     = { enabled = false },
        pycodestyle = { enabled = false },
        pyflakes    = { enabled = false },
        pylint      = { enabled = false },
        yapf        = { enabled = false },
      },
    },
  },
}

-- local pyright = {
--   on_attach = require("core.func").LSPAttach,
--   filetypes = { "python" },
--   root_patterns = { "*.py" },
--   settings = {
--     python = {
--       analysis = {
--         autoSearchPaths = true,
--         diagnosticMode = "openFilesOnly",
--         useLibraryCodeForTypes = true,
--       },
--     },
--   },
--   cmd = { "pyright-langserver", "--stdio" },
-- }

local pylyzer = {
  on_attach = require("core.func").LSPAttach,
  cmd = { "pylyzer", "--server" },
  filetypes = { "python" },
  root_patterns = { "*.py" },
  settings = {
    python = {
      checkOnType = false,
      diagnostics = true,
      inlayHints = true,
      smartCompletion = true,
    },
  },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(pylsp) end
  )
end)

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(ruff) end
  )
end)
