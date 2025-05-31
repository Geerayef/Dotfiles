-- OCaml OCP indent

local ocp_indent_path =
  vim.fn.substitute(vim.fn.system("opam var ocp-indent:share"), "\n$", "", "")
ocp_indent_path = ocp_indent_path .. "/vim"
vim.opt.rtp:append(ocp_indent_path)

-- Options

vim.g.no_ocaml_maps = 1
vim.o.makeprg = "dune build @ocaml-index"
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.g.sleuth_ocaml_heuristics = 0
vim.tresitter.start(vim.fn.bufnr(), "ocaml")
