---@type vim.lsp.Config
local hls = {
  name = "Haskell LS",
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  on_attach = LSP.attach,
  filetypes = { "haskell", "lhaskell", "cabal" },
  root_markers = { "*.hs" },
  settings = {
    haskell = { cabalFormattingProvider = "", formattingProvider = "" },
  },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(hls) end
  )
end)
