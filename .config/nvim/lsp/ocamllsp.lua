---@type vim.lsp.Config
return {
  name = "OCaml LS",
  cmd = { "ocamllsp" },
  capabilities = { handleSwitchImplIntf = true },
  filetypes = { "ocaml", "opam", "dune" },
  root_markers = { "*.ml", "*.mli", "*.opam", "*.dune" },
}
