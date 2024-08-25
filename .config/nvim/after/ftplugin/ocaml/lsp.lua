local ocamlls = {
  on_attach = require("core.func").LspAttach,
  capabilities = { handleSwitchImplIntf = true },
  filetypes = { "ocaml", "ocamlinterface", "opam", "dune" },
  root_patterns = { "*.ml", "*.mli", "*.opam" },
  cmd = { "ocamllsp" },
}

LSP.start(ocamlls)
