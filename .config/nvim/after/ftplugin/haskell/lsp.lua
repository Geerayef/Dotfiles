local hls = {
  on_attach = require("core.func").LSPAttach,
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_patterns = { "*.cabal" },
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  settings = {
    haskell = {
      cabalFormattingProvider = "cabal-gild",
      formattingProvider = "fourmolu",
    },
  },
}
