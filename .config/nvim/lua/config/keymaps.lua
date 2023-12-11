vim.g.mapleader = " "

local keymap = vim.keymap.set
local noremap = { noremap = true }
local silent = { silent = true }

-- ~  General keymaps

keymap("v", "J", ":m '>+1<CR>gv=gv", noremap)
keymap("v", "K", ":m '<-2<CR>gv=gv", noremap)
keymap("n", "<C-d>", "10<C-d>", noremap)
keymap("n", "<C-u>", "10<C-u>", noremap)

-- Buffer
keymap("n", "<leader>bd", "<cmd>bdelete<CR>")

-- Search
keymap("n", "<leader>nh", "<cmd>nohl<CR>")
keymap("n", "n", "nzzzv")
keymap("n", "N", "Nzzzv")

-- Copy/Yank/Paste
keymap("n", "x", '"_x')
keymap("x", "<leader>P", '"_dP')
keymap("n", "<leader>y", '"+y')
keymap("n", "<leader>Y", '"+Y')
keymap("v", "<leader>y", '"+y')

-- Window Splitting
keymap("n", "<leader>sv", "<C-w>v", { desc = "[S]plit [V]ertically" })
keymap("n", "<leader>sh", "<C-w>s", { desc = "[S]plit [H]orizontally" })
keymap("n", "<leader>se", "<C-w>=", { desc = "[S]plit [E]qualize sizes" })
keymap("n", "<leader>spx", "<cmd>close<CR>", { desc = "[S]plit [X] close" })

-- Tabs
keymap("n", "<leader>to", "<cmd>tabnew<CR>"     , { desc = "[T]ab [O]pen" })
keymap("n", "<leader>tn", "<cmd>tabnext<CR>"    , { desc = "[T]ab [N]ext" })
keymap("n", "<leader>tp", "<cmd>tabprevious<CR>", { desc = "[T]ab [P]revious" })
keymap("n", "<leader>tml", "<cmd>tabmove +1<CR>", { desc = "[T]ab [M]ove [L]eft" })
keymap("n", "<leader>tmr", "<cmd>tabmove -1<CR>", { desc = "[T]ab [M]ove [R]ight" })

-- Terminal
keymap("t", "<Esc>", "<C-\\><C-n>", { desc = "Terminal mode: Escape" })

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps

-- Telescope
keymap("n", "<leader>?", "<cmd>Telescope oldfiles<CR>"     , noremap, { desc = "[?] Recent files" })
keymap("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", noremap, { desc = "[/] Search buffer" })
keymap("n", "<leader>fb", "<cmd>Telescope file_browser<CR>", noremap, { desc = "[F]ile [B]rowser" })
keymap("n", "<leader>tb", "<cmd>Telescope builtin<CR>"     , noremap, { desc = "[T]elescope [B]uiltin" })
keymap("n", "<leader>tsf", "<cmd>Telescope find_files<CR>" , noremap, { desc = "[T]elescope [S]earch [F]iles" })
keymap("n", "<leader>tsh", "<cmd>Telescope help_tags<CR>"  , noremap, { desc = "[T]elescope [S]earch [H]elp" })
keymap("n", "<leader>tsw", "<cmd>Telescope grep_string<CR>", noremap, { desc = "[T]elescope [S]earch [W]ord" })
keymap("n", "<leader>tsg", "<cmd>Telescope live_grep<CR>"  , noremap, { desc = "[T]elescope [S]earch [G]rep" })
keymap("n", "<leader>tsd", "<cmd>Telescope diagnostics<CR>", noremap, { desc = "[T]elescope [S]earch [D]iagnostics" })
keymap("n", "<leader><space>", "<cmd>Telescope buffers<CR>", noremap, { desc = "[ ] Opened buffers" })

-- Gitsigns
keymap("n", "<leader>glb", "<cmd>Gitsigns toggle_current_line_blame<CR>", { desc = "[G]it [L]ine [B]lame"})

-- Diagnostic keymaps
keymap("n", "<leader>dn", vim.diagnostic.goto_next, noremap, { desc = "[D]iagnostic [N]ext" })
keymap("n", "<leader>dp", vim.diagnostic.goto_prev, noremap, { desc = "[D]iagnostic [P]reivous" })
keymap("n", "<leader>df", "<cmd>lua vim.diagnostic.open_float()<CR>", silent, { desc = "[D]iagnostic [F]loat" })

-- Zen
keymap("n", "<leader>zn", "<cmd>TZNarrow<CR>"     , noremap, { desc = "[Z]en [N]arrow" })
keymap("v", "<leader>zn", "<cmd>'<,'>TZNarrow<CR>", noremap, { desc = "[Z]en [N]arrow selection" })
keymap("n", "<leader>zf", "<cmd>TZFocus<CR>"      , noremap, { desc = "[Z]en [F]ocus" })
keymap("n", "<leader>zm", "<cmd>TZMinimalist<CR>" , noremap, { desc = "[Z]en [M]inimalist" })
keymap("n", "<leader>za", "<cmd>TZAtaraxis<CR>"   , noremap, { desc = "[Z]en [A]taraxis" })

-- Tmux
-- keymap("n", "<C-h>", "<cmd>TmuxNavigateLeft<CR>",  opt_noremap, { desc = "Tmux Navigate Left" })
-- keymap("n", "<C-j>", "<cmd>TmuxNavigateDown<CR>",  opt_noremap, { desc = "Tmux Navigate Down" })
-- keymap("n", "<C-k>", "<cmd>TmuxNavigateUp<CR>",    opt_noremap, { desc = "Tmux Navigate Up" })
-- keymap("n", "<C-l>", "<cmd>TmuxNavigateRight<CR>", opt_noremap, { desc = "Tmux Navigate Right" })
