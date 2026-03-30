---@type vim.lsp.Config
return {
  name = "Haskell LS",
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_markers = { "*.hs" },
  settings = { haskell = { cabalFormattingProvider = "", formattingProvider = "" } },
}
