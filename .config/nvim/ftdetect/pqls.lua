vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  desc = "Detect PQLS file type.",
  pattern = "*.pqls",
  callback = function() vim.cmd.setfiletype({ args = { "pqls" } }) end,
})
