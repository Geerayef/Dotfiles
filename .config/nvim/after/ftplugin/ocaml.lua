-- ~  OCaml merlin integration

vim.g.opamshare = vim.fn.substitute(vim.fn.system("opam var share"), "\n$", "", "")
local rtp = vim.o.runtimepath
local ocp_indent_path = vim.g.opamshare .. "ocp-indent/vim"
local merlin_path = vim.g.opamshare .. "/merlin/vim"

rtp = rtp .. "," .. ocp_indent_path
rtp = rtp .. "," .. merlin_path

vim.cmd(':execute "helptags ".g:opamshare."/merlin/vim/doc"')

