---@type vim.lsp.Config
return {
  name = "OCaml LS",
  cmd = { "ocamllsp" },
  capabilities = { handleSwitchImplIntf = true },
  filetypes = { "ocaml", "opam", "dune" },
  root_markers = GRIM.static.root_markers.ocaml,
}
