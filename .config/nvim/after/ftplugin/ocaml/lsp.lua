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

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(ocamlls) end
  )
end)
