-- ~  OCaml merlin integration
vim.cmd[[set rtp^="/home/tibor/.opam/default/share/ocp-indent/vim"]]
vim.cmd[[let g:opamshare = substitute(system('opam var share'),'\n$','','''')]]
vim.cmd[[set rtp+=g:opamshare."/merlin/vim"]]
vim.cmd[[:execute "helptags " .g:opamshare."/merlin/vim/doc"]]

