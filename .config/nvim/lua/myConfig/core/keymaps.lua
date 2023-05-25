-- #  Leader
vim.g.mapleader = " "

local keymap = vim.keymap

-- #  General keymaps

keymap.set("v", "J", ":m '>+1<CR>gv=gv")
keymap.set("v", "K", ":m '<-2<CR>gv=gv")
keymap.set("n", "<C-d>", "10<C-d>", { noremap = true })
keymap.set("n", "<C-u>", "10<C-u>", { noremap = true })

-- #  Search
keymap.set("n", "<leader>nh", ":nohl<CR>")
keymap.set("n", "n", "nzzzv")
keymap.set("n", "N", "Nzzzv")

-- #  Copy/Yank/Paste
keymap.set("n", "x", '"_x')
keymap.set("x", "<leader>P", '"_dP')
keymap.set("n", "<leader>y", '"+y')
keymap.set("n", "<leader>Y", '"+Y')
keymap.set("v", "<leader>y", '"+y')

-- #  Window Splitting
keymap.set("n", "<leader>spv", "<C-w>v")
keymap.set("n", "<leader>sph", "<C-w>s")
keymap.set("n", "<leader>spe", "<C-w>=")
keymap.set("n", "<leader>spx", ":close<CR>")

-- #  Tabs
keymap.set("n", "<leader>to", ":tabnew<CR>")
keymap.set("n", "<leader>tx", ":q!<CR>")
keymap.set("n", "<leader>tn", ":tabn<CR>")
keymap.set("n", "<leader>tp", ":tabp<CR>")

-- # Plugin keymaps

-- nvim-tree
keymap.set("n", "<leader>e", ":NvimTreeToggle<CR>")

-- Telescope
-- See `:help telescope.builtin`
keymap.set("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", { desc = "[?] Find recently opened files" })
keymap.set("n", "<leader><space>", "<cmd>Telescope buffers<CR>", { desc = "[ ] Find existing buffers" })
keymap.set("n", "<leader>/", function()
  require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown {
    winblend = 1,
    previewer = true,
  })
end, { desc = "[/] Fuzzily search in current buffer]" })

-- Telescope shortcuts start with t
keymap.set("n", "<leader>tb", "<cmd>Telescope builtin<CR>", { desc = "[T]elescope [B]uiltin" })
keymap.set("n", "<leader>tsf", "<cmd>Telescope find_files<CR>", { desc = "[S]earch [F]iles" })
keymap.set("n", "<leader>tsh", "<cmd>Telescope help_tags<CR>", { desc = "[S]earch [H]elp" })
keymap.set("n", "<leader>tsw", "<cmd>Telescope grep_string<CR>", { desc = "[S]earch current [W]ord" })
keymap.set("n", "<leader>tsg", "<cmd>Telescope live_grep<CR>", { desc = "[S]earch by [G]rep" })
keymap.set("n", "<leader>tsd", "<cmd>Telescope diagnostics<CR>", { desc = "[S]earch [D]iagnostics" })

-- #  Diagnostic keymaps
keymap.set("n", "[d", vim.diagnostic.goto_prev)
keymap.set("n", "]d", vim.diagnostic.goto_next)
keymap.set("n", "e?", "<cmd>lua vim.diagnostic.open_float()<CR>", { silent = true, })

-- #  Breadcrumbs
keymap.set("n", "<leader>bc", "<cmd>Navbuddy<CR>")
