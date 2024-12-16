local ocamlls = {
  on_attach = require("core.func").LSPAttach,
  capabilities = { handleSwitchImplIntf = true },
  filetypes = { "ocaml", "ocamlinterface", "opam", "dune" },
  root_patterns = {
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
  cmd = { "ocamllsp" },
}

require("util.lsp").start(ocamlls)
