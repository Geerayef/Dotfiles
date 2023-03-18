-- # Leader
vim.g.mapleader = " "

local keymap = vim.keymap

-- # General keymaps

keymap.set("v", "J", ":m '>+1<CR>gv=gv")
keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- # Search
-- Clear search highlights
keymap.set("n", "<leader>nh", ":nohl<CR>") -- Clear search highlights
keymap.set("n", "n", "nzzzv")
keymap.set("n", "N", "Nzzzv")

-- # Copy/Yank/Paste
-- x wont copy deleted character to register
keymap.set("n", "x", '"_x')
keymap.set("x", "<leader>P", "\"_dP")
keymap.set("n", "<leader>y", "\"+y")
keymap.set("n", "<leader>Y", "\"+Y")
keymap.set("v", "<leader>y", "\"+y")

-- # Window Splitting
keymap.set("n", "<leader>spv", "<C-w>v") -- vertical
keymap.set("n", "<leader>sph", "<C-w>s") -- horizontal
keymap.set("n", "<leader>spe", "<C-w>=") -- equalize width
keymap.set("n", "<leader>spx", ":close<CR>") -- close current
-- # Tabs
keymap.set("n", "<leader>to", ":tabnew<CR>")
keymap.set("n", "<leader>tx", ":tabclose<CR>")
keymap.set("n", "<leader>tn", ":tabn<CR>")
keymap.set("n", "<leader>tp", ":tabp<CR>")

-- # Plugin keymaps

-- nvim-tree
keymap.set("n", "<leader>e", ":NvimTreeToggle<CR>")

-- telescope
-- See `:help telescope.builtin`
keymap.set("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", { desc = "[?] Find recently opened files" })
keymap.set("n", "<leader><space>", "<cmd>Telescope buffers<CR>", { desc = "[ ] Find existing buffers" })
keymap.set("n", "<leader>/", function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown {
    winblend = 1,
    previewer = true,
  })
end, { desc = "[/] Fuzzily search in current buffer]" })

keymap.set("n", "<leader>sf", "<cmd>Telescope find_files<CR>", { desc = "[S]earch [F]iles" })
keymap.set("n", "<leader>sh", "<cmd>Telescope help_tags<CR>", { desc = "[S]earch [H]elp" })
keymap.set("n", "<leader>sw", "<cmd>Telescope grep_string<CR>", { desc = "[S]earch current [W]ord" })
keymap.set("n", "<leader>sg", "<cmd>Telescope live_grep<CR>", { desc = "[S]earch by [G]rep" })
keymap.set("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>", { desc = "[S]earch [D]iagnostics" })

-- Diagnostic keymaps
keymap.set("n", "[d", vim.diagnostic.goto_prev)
keymap.set("n", "]d", vim.diagnostic.goto_next)
keymap.set("n", "g?", "<cmd>lua vim.diagnostic.open_float()<CR>", { silent = true, })

