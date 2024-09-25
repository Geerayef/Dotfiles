-- OCaml OCP indent

local ocp_indent_path =
  vim.fn.substitute(vim.fn.system("opam var ocp-indent:share"), "\n$", "", "")
ocp_indent_path = ocp_indent_path .. "/vim"
vim.opt.rtp:append(ocp_indent_path)

-- Options

vim.o.makeprg = "dune build"
vim.o.tabstop = 2
vim.o.softtabstop = -1
vim.o.shiftwidth = 2
