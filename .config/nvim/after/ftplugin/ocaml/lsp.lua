local lsp = require("util.lsp")
local ocamlls = {
  filetypes = { "ocaml", "ocamlinterface", "opam", "dune" },
  root_patterns = { "*.ml", "*.mli", "*.opam" },
  cmd = { "ocamllsp" },
}

if lsp.start(ocamlls) ~= nil then
  F.Notify("INFO", ocamlls.cmd[1] .. " LSP started.")
else
  F.Notify("ERROR", "Could not start LSP " .. ocamlls.cmd[1])
end
