---@type vim.lsp.Config
return {
  name = "Haskell LS",
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_markers = GRIM.static.root_markers.haskell,
  settings = { haskell = { cabalFormattingProvider = "", formattingProvider = "" } },
}
