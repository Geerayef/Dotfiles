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
keymap("n", "<leader>tmr", "<cmd>tabmove +1<CR>", { desc = "[T]ab [M]ove [L]eft" })
keymap("n", "<leader>tml", "<cmd>tabmove -1<CR>", { desc = "[T]ab [M]ove [R]ight" })

-- Terminal
keymap("t", "<Esc>", "<C-\\><C-n>", { desc = "Terminal mode: Escape" })

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps

-- Telescope
keymap("n", "<leader>?", "<cmd>Telescope oldfiles<CR>"     , noremap, { desc = "[?] Recent files" })
keymap("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", noremap, { desc = "[/] Search buffer" })
keymap("n", "<leader>fb", "<cmd>Telescope file_browser<CR>", noremap, { desc = "[F]ile [B]rowser" })
keymap("n", "<leader>tb", "<cmd>Telescope builtin<CR>"     , noremap, { desc = "Telescope [B]uiltin" })
keymap("n", "<leader>sf", "<cmd>Telescope find_files<CR>"  , noremap, { desc = "Telescope [S]earch [F]iles" })
keymap("n", "<leader>sh", "<cmd>Telescope help_tags<CR>"   , noremap, { desc = "Telescope [S]earch [H]elp" })
keymap("n", "<leader>sw", "<cmd>Telescope grep_string<CR>" , noremap, { desc = "Telescope [S]earch [W]ord" })
keymap("n", "<leader>sg", "<cmd>Telescope live_grep<CR>"   , noremap, { desc = "Telescope [S]earch [G]rep" })
keymap("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>" , noremap, { desc = "Telescope [S]earch [D]iagnostics" })
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

-------------------------------------------------------------------------------------------------------

-- ~  LSP keymaps

Keymaps = {}

Keymaps.LSP = function(client, bufnr)
  local lspbuf = vim.lsp.buf
  keymap("n", "<leader>rn", lspbuf.rename, { buffer = bufnr, desc = "[R]e[n]ame" })
  keymap("n", "<leader>ca", lspbuf.code_action, { buffer = bufnr, desc = "[C]ode [A]ction" })
  keymap("n", "<leader>gd", lspbuf.definition, { buffer = bufnr, desc = "[G]oto [d]efinition" })
  keymap("n", "<leader>gD", lspbuf.declaration, { buffer = bufnr, desc = "[G]oto [D]eclaration" })
  keymap("n", "<leader>gr", "<cmd>Telescope lsp_references<CR>", { buffer = bufnr, desc = "[G]oto [R]eferences" })
  keymap("n", "<leader>gi", lspbuf.implementation, { buffer = bufnr, desc = "[G]oto [I]mplementation" })
  keymap("n", "<leader>D" , lspbuf.type_definition, { buffer = bufnr, desc = "Type [D]efinition" })
  keymap("n", "<leader>ds", "<cmd>Telescope lsp_document_symbols<CR>", { buffer = bufnr, desc = "[D]ocument [S]ymbols" })
  keymap("n", "<leader>ws", "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", { buffer = bufnr, desc = "[W]orkspace [S]ymbols" })
  keymap("n", "<leader>waf", lspbuf.add_workspace_folder, { buffer = bufnr, desc = "[W]orkspace [A]dd [F]older" })
  keymap("n", "<leader>wrf", lspbuf.remove_workspace_folder, { buffer = bufnr, desc = "[W]orkspace [R]emove [F]older" })
  keymap("n", "<leader>wlf", "<cmd>lua = print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", { buffer = bufnr, desc = "[W]orkspace [L]ist [F]olders" })
  keymap("n", "K", lspbuf.hover, { buffer = bufnr, desc = "Hover Documentation" })
  keymap("n", "<C-k>", lspbuf.signature_help, { buffer = bufnr, desc = "Signature Documentation" })
end

return Keymaps
