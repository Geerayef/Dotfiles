local hls = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_patterns = { "*.hs", "*.cabal", "hie.yaml", "*.project" },
  cmd = { "haskell-language-server-wrapper", "--lsp", "-j", "4" },
  settings = {
    haskell = { cabalFormattingProvider = "", formattingProvider = "" },
  },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(hls) end
  )
end)
