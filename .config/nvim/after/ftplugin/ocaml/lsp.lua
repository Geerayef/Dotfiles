local lsp = require("util.lsp")
local ocamlls = {
  filetypes = { "ocaml", "ocamlinterface", "opam", "dune" },
  root_patterns = { "*.ml", "*.mli", "*.opam" },
  cmd = { "ocamllsp" },
}
lsp.start(ocamlls)
