-- ~  OCaml merlin integration

local keymap = vim.api.nvim_buf_set_keymap

keymap(
  0,
  "n",
  "<leader>duf",
  "<cmd>!dune fmt<CR>",
  F.KeymapArgs({ silent = true, desc = "[Du]ne [f]ormat current project." })
)
-- keymap(0, "n", "<leader>ocf", "<cmd>!ocamlformat --inplace %<CR>", F.KeymapArgs({ silent = true, desc = "[oc]aml[f]ormat current buffer." }))

vim.g.opamshare = vim.fn.substitute(vim.fn.system("opam var share"), "\n$", "", "")
local ocp_indent_path = vim.g.opamshare .. "ocp-indent/vim"
local merlin_path = vim.g.opamshare .. "/merlin/vim"
vim.opt.rtp:append(ocp_indent_path)
vim.opt.rtp:append(merlin_path)
vim.cmd({ cmd = "helptags", args = { vim.g.opamshare .. "/merlin/vim/doc" } })
