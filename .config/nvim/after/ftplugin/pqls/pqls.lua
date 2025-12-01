vim.treesitter.language.add("pqls")
vim.treesitter.language.register("pqls", { "pqls" })
vim.treesitter.start()
-- ~ Options
vim.opt.commentstring = "// %s"
-- ~ Keymaps
vim.keymap.set(
  "n",
  "<leader>PQL",
  function() require("lua.grim.pql")(vim.api.nvim_get_current_buf()) end,
  { buffer = true, desc = "Execute current PQLS buffer." }
)
