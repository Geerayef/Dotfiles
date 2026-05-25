---@type vim.lsp.Config
return {
  name = "Haskell LS",
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  filetypes = { "haskell", "lhaskell", "cabal" },
  settings = { haskell = { cabalFormattingProvider = "", formattingProvider = "" } },
}
