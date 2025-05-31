vim.keymap.set(
  "n",
  "<leader>vc",
  "<cmd>VimtexCompile<CR>",
  { noremap = true, silent = true, desc = "[v]imtex [c]ompile" }
)
vim.keymap.set(
  "n",
  "<leader>vi",
  "<cmd>VimtexInfo<CR>",
  { noremap = true, silent = true, desc = "[v]imtex [i]nfo" }
)
vim.keymap.set(
  "n",
  "<leader>vv",
  "<cmd>VimtexView<CR>",
  { noremap = true, silent = true, desc = "[v]imtex [v]iew" }
)
vim.treesitter.start(vim.fn.bufnr(), "tex")
