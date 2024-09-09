-- OCaml OCP indent

local ocp_indent_path =
  vim.fn.substitute(vim.fn.system("opam var ocp-indent:share"), "\n$", "", "")
ocp_indent_path = ocp_indent_path .. "/vim"
vim.opt.rtp:append(ocp_indent_path)

-- Options

vim.opt.makeprg = "dune build"
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
