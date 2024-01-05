-- ~  OCaml merlin integration


local keymap = vim.api.nvim_buf_set_keymap

keymap(0, "n", "<leader>fmt", "<cmd>!dune fmt<CR>", F.KeymapArgs({ silent = true, desc = "[f]or[m]a[t] current project with ocamlformat" }))

-- vim.g.opamshare = vim.fn.substitute(vim.fn.system("opam var share"), "\n$", "", "")
-- local ocp_indent_path = vim.g.opamshare .. "ocp-indent/vim"
-- local merlin_path = vim.g.opamshare .. "/merlin/vim"
-- vim.opt.rtp:append(ocp_indent_path)
-- vim.opt.rtp:append(merlin_path)
-- vim.cmd(':execute "helptags ".g:opamshare."/merlin/vim/doc"')
