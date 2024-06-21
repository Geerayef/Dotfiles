local ocamlls = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "ocaml", "ocamlinterface", "opam", "dune" },
  root_patterns = { "*.ml", "*.mli", "*.opam" },
  cmd = { "ocamllsp" },
}

require("util.lsp").start(ocamlls)
