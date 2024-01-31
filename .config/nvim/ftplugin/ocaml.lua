-- ~  OCaml Merlin & OCP indent

vim.g.opamshare = vim.fn.substitute(vim.fn.system("opam var share"), "\n$", "", "")
local merlin_path = vim.g.opamshare .. "/merlin/vim"
vim.opt.rtp:append(merlin_path)
local merlin_doc = merlin_path .. "/doc"
vim.cmd({ cmd = "helptags", args = { merlin_doc } })
local ocp_indent_path = vim.fn.substitute(vim.fn.system("opam var ocp-indent:share"), "\n$", "", "")
vim.opt.rtp:append(ocp_indent_path)
