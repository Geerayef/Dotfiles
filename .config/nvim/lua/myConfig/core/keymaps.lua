vim.g.mapleader = " "
local keymap = vim.keymap.set
local truezen = require("true-zen")

-------------------------------------------------------------------------------------------------------

-- ~  General keymaps

keymap("v", "J", ":m '>+1<CR>gv=gv")
keymap("v", "K", ":m '<-2<CR>gv=gv")
keymap("n", "<C-d>", "10<C-d>", { noremap = true })
keymap("n", "<C-u>", "10<C-u>", { noremap = true })

-- Search
keymap("n", "<leader>nh", ":nohl<CR>")
keymap("n", "n", "nzzzv")
keymap("n", "N", "Nzzzv")

-- Copy/Yank/Paste
keymap("n", "x", '"_x')
keymap("x", "<leader>P", '"_dP')
keymap("n", "<leader>y", '"+y')
keymap("n", "<leader>Y", '"+Y')
keymap("v", "<leader>y", '"+y')

-- Window Splitting
keymap("n", "<leader>spv", "<C-w>v")
keymap("n", "<leader>sph", "<C-w>s")
keymap("n", "<leader>spe", "<C-w>=")
keymap("n", "<leader>spx", ":close<CR>")

-- Tabs
keymap("n", "<leader>to", "<cmd>tabnew<CR>")
keymap("n", "<leader>tx", "<cmd>q!<CR>")
keymap("n", "<leader>tn", "<cmd>tabn<CR>")
keymap("n", "<leader>tp", "<cmd>tabp<CR>")
keymap("n", "<leader>tml", "<cmd>tabmove +1<CR>")
keymap("n", "<leader>tmr", "<cmd>tabmove -1<CR>")

-- Terminal
keymap("t", "<Esc>", "<C-\\><C-n>")

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps

-- nvim-tree
keymap("n", "<leader>e", ":NvimTreeToggle<CR>")

-- Telescope - see `:help telescope.builtin`
keymap("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", { desc = "[?] Find recently opened files" })
keymap("n", "<leader><space>", "<cmd>Telescope buffers<CR>", { desc = "[ ] Find existing buffers" })
keymap("n", "<leader>/", function()
  require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown {
    winblend = 1,
    previewer = true,
  })
end, { desc = "[/] Fuzzily search in current buffer]" })
keymap("n", "<leader>tb", "<cmd>Telescope builtin<CR>", { desc = "[T]elescope [B]uiltin" })
keymap("n", "<leader>tsf", "<cmd>Telescope find_files<CR>", { desc = "[S]earch [F]iles" })
keymap("n", "<leader>tsh", "<cmd>Telescope help_tags<CR>", { desc = "[S]earch [H]elp" })
keymap("n", "<leader>tsw", "<cmd>Telescope grep_string<CR>", { desc = "[S]earch current [W]ord" })
keymap("n", "<leader>tsg", "<cmd>Telescope live_grep<CR>", { desc = "[S]earch by [G]rep" })
keymap("n", "<leader>tsd", "<cmd>Telescope diagnostics<CR>", { desc = "[S]earch [D]iagnostics" })

-- Git
keymap("n", "<leader>gbt", "<cmd>Gitsigns toggle_current_line_blame<CR>")

-- ~  Diagnostic keymaps
keymap("n", "[d", vim.diagnostic.goto_prev)
keymap("n", "]d", vim.diagnostic.goto_next)
keymap("n", "e?", "<cmd>lua vim.diagnostic.open_float()<CR>", { silent = true, })

-- ~  Breadcrumbs
keymap("n", "<leader>bc", "<cmd>Navbuddy<CR>")

-- ~ Zen
keymap("n", "<leader>zn", function()
  local first = 0
  local last = vim.api.nvim_buf_line_count(0)
  truezen.narrow(first, last)
end, { noremap = true })
keymap("v", "<leader>zn", function()
  local first = vim.fn.line("v")
  local last = vim.fn.line(".")
  truezen.narrow(first, last)
end, { noremap = true })
keymap("n", "<leader>zf", truezen.focus, { noremap = true })
keymap("n", "<leader>zm", truezen.minimalist, { noremap = true })
keymap("n", "<leader>za", truezen.ataraxis, { noremap = true })
