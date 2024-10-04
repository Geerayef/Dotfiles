local ocamlls = {
  on_attach = require("core.func").LspAttach,
  capabilities = { handleSwitchImplIntf = true },
  filetypes = { "ocaml", "ocamlinterface", "opam", "dune" },
  root_patterns = {
    "*.ml",
    "*.mli",
    "*.opam",
    ".opam",
    ".ocamlformat",
    ".ocp-indent",
    "dune",
    "dune-project",
    "dune-workspace",
  },
  cmd = { "ocamllsp" },
}

require("util.lsp").start(ocamlls)
