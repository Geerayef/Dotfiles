local ocamlls = {
  name = "OCaml LS",
  cmd = { "ocamllsp" },
  on_attach = LSP.attach,
  capabilities = { handleSwitchImplIntf = true },
  filetypes = { "ocaml", "ocamlinterface", "opam", "dune" },
  root_markers = {
    "*.ml",
    "*.mli",
    "*.opam",
    "*.dune",
    "dune",
    "dune-project",
    "dune-workspace",
    ".ocamlformat",
    ".ocp-indent",
    ".opam",
  },
}

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(ocamlls) end
  )
end)
