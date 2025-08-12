local hls = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_patterns = { "*.hs", "*.cabal" },
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  settings = {
    haskell = {
      cabalFormattingProvider = "",
      formattingProvider = "",
    },
  },
}
