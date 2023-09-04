vim.g.mapleader = " "

local keymap = vim.keymap.set

-- ~  General keymaps

keymap("v", "J", ":m '>+1<CR>gv=gv")
keymap("v", "K", ":m '<-2<CR>gv=gv")
keymap("n", "<C-d>", "10<C-d>", { noremap = true })
keymap("n", "<C-u>", "10<C-u>", { noremap = true })

-- Buffer
keymap("n", "<leader>bd", "<cmd>bdelete<CR>")

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
keymap("n", "<leader>tn", "<cmd>tabn<CR>")
keymap("n", "<leader>tp", "<cmd>tabp<CR>")
keymap("n", "<leader>tml", "<cmd>tabmove +1<CR>")
keymap("n", "<leader>tmr", "<cmd>tabmove -1<CR>")

-- Terminal
keymap("t", "<Esc>", "<C-\\><C-n>")

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps


-- Telescope
keymap("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", { desc = "[?] Find recently opened files" })
keymap("n", "<leader><space>", "<cmd>Telescope buffers<CR>", { desc = "[ ] Find existing buffers" })
keymap("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", { desc = "[/] Fuzzily search in current buffer]" })
keymap("n", "<leader>tb", "<cmd>Telescope builtin<CR>", { desc = "[T]elescope [B]uiltin" })
keymap("n", "<leader>tsf", "<cmd>Telescope find_files<CR>", { desc = "[S]earch [F]iles" })
keymap("n", "<leader>tsh", "<cmd>Telescope help_tags<CR>", { desc = "[S]earch [H]elp" })
keymap("n", "<leader>tsw", "<cmd>Telescope grep_string<CR>", { desc = "[S]earch current [W]ord" })
keymap("n", "<leader>tsg", "<cmd>Telescope live_grep<CR>", { desc = "[S]earch by [G]rep" })
keymap("n", "<leader>tsd", "<cmd>Telescope diagnostics<CR>", { desc = "[S]earch [D]iagnostics" })
keymap("n", "<leader>fb", "<cmd>Telescope file_browser<CR>", { desc = "[F]ile [B]rowser" }, { noremap = true })

-- Git
keymap("n", "<leader>gbt", "<cmd>Gitsigns toggle_current_line_blame<CR>", { desc = "[G]it [B]lame [T]oggle"})

-- Diagnostic keymaps
keymap("n", "[d", vim.diagnostic.goto_prev)
keymap("n", "]d", vim.diagnostic.goto_next)
keymap("n", "<leader>df", "<cmd>lua vim.diagnostic.open_float()<CR>", { silent = true, })

-- Zen
keymap("n", "<leader>zn", "<cmd>TZNarrow<CR>", { noremap = true })
keymap("v", "<leader>zn", "<cmd>'<,'>TZNarrow<CR>", { noremap = true })
keymap("n", "<leader>zf", "<cmd>TZFocus<CR>", { noremap = true })
keymap("n", "<leader>zm", "<cmd>TZMinimalist<CR>", { noremap = true })
keymap("n", "<leader>za", "<cmd>TZAtaraxis<CR>", { noremap = true })

-- Tmux
keymap("n", "<C-h>", "<cmd>TmuxNavigateLeft<CR>", { noremap = true })
keymap("n", "<C-j>", "<cmd>TmuxNavigateDown<CR>", { noremap = true })
keymap("n", "<C-k>", "<cmd>TmuxNavigateUp<CR>", { noremap = true })
keymap("n", "<C-l>", "<cmd>TmuxNavigateRight<CR>", { noremap = true })
