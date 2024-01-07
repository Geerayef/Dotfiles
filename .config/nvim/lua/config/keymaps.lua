vim.g.mapleader = " "

local keymap = vim.keymap.set

-- ~  General keymaps

keymap("x", "J", ":m '>+1<CR>gv=gv")
keymap("x", "K", ":m '<-2<CR>gv=gv")
keymap("n", "<C-d>", "10<C-d>")
keymap("n", "<C-u>", "10<C-u>")

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
keymap("n", "<leader>spv", "<C-w>v", F.KeymapArgs({ desc = "[S]plit [V]ertically" }))
keymap("n", "<leader>sph", "<C-w>s", F.KeymapArgs({ desc = "[S]plit [H]orizontally" }))
keymap("n", "<leader>spe", "<C-w>=", F.KeymapArgs({ desc = "[S]plit [E]qualize sizes" }))
keymap("n", "<leader>spx", "<cmd>close<CR>", F.KeymapArgs({ desc = "[S]plit [X] close" }))

-- Tabs
keymap("n", "<leader>to", "<cmd>tabnew<CR>"     , F.KeymapArgs({ desc = "[T]ab [O]pen" }))
keymap("n", "<leader>tn", "<cmd>tabnext<CR>"    , F.KeymapArgs({ desc = "[T]ab [N]ext" }))
keymap("n", "<leader>tp", "<cmd>tabprevious<CR>", F.KeymapArgs({ desc = "[T]ab [P]revious" }))
keymap("n", "<leader>tmr", "<cmd>tabmove +1<CR>", F.KeymapArgs({ desc = "[T]ab [M]ove [L]eft" }))
keymap("n", "<leader>tml", "<cmd>tabmove -1<CR>", F.KeymapArgs({ desc = "[T]ab [M]ove [R]ight" }))

-- Terminal
keymap("t", "<Esc>", "<C-\\><C-n>", { desc = "Terminal mode: Escape" })

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps

-- Telescope
keymap("n", "<leader>?", "<cmd>Telescope oldfiles<CR>"     , F.KeymapArgs({ desc = "[?] Recent files" }))
keymap("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", F.KeymapArgs({ desc = "[/] Search buffer" }))
keymap("n", "<leader>f", "<cmd>Telescope file_browser<CR>", F.KeymapArgs({ desc = "Telescope [F]ile Browser" }))
keymap("n", "<leader>tb", "<cmd>Telescope builtin<CR>"     , F.KeymapArgs({ desc = "Telescope [B]uiltin" }))
keymap("n", "<leader>sf", "<cmd>Telescope find_files<CR>"  , F.KeymapArgs({ desc = "Telescope [S]earch [F]iles" }))
keymap("n", "<leader>sh", "<cmd>Telescope help_tags<CR>"   , F.KeymapArgs({ desc = "Telescope [S]earch [H]elp" }))
keymap("n", "<leader>sw", "<cmd>Telescope grep_string<CR>" , F.KeymapArgs({ desc = "Telescope [S]earch [W]ord" }))
keymap("n", "<leader>sg", "<cmd>Telescope live_grep<CR>"   , F.KeymapArgs({ desc = "Telescope [S]earch [G]rep" }))
keymap("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>" , F.KeymapArgs({ desc = "Telescope [S]earch [D]iagnostics" }))
keymap("n", "<leader><space>", "<cmd>Telescope buffers<CR>", F.KeymapArgs({ desc = "[ ] Opened buffers" }))

-- Gitsigns
keymap("n", "<leader>glb", "<cmd>Gitsigns toggle_current_line_blame<CR>", F.KeymapArgs({ desc = "[G]it [L]ine [B]lame"}))
-- Fugitive
keymap("n", "<leader>G",   "<cmd>Git<CR>",  F.KeymapArgs({ desc = "[G]it [s]tatus." }))
keymap("n", "<leader>gab",  "<cmd>Git add %<CR>", F.KeymapArgs({ desc = "[G]it [a]dd." }))
keymap("n", "<leader>gpl", "<cmd>Git pull<CR>", F.KeymapArgs({ desc = "[G]it [p]u[l]l." }))
keymap("n", "<leader>gps", "<cmd>Git push<CR>", F.KeymapArgs({ desc = "[G]it [p]u[s]h." }))

-- Diagnostic keymaps
keymap("n", "<leader>dn", vim.diagnostic.goto_next, F.KeymapArgs({ desc = "[D]iagnostic [N]ext" }))
keymap("n", "<leader>dp", vim.diagnostic.goto_prev, F.KeymapArgs({ desc = "[D]iagnostic [P]reivous" }))

-- Zen
keymap("n", "<leader>zn", "<cmd>TZNarrow<CR>"     , F.KeymapArgs({ desc = "[Z]en [N]arrow" }))
keymap("v", "<leader>zn", "<cmd>'<,'>TZNarrow<CR>", F.KeymapArgs({ desc = "[Z]en [N]arrow selection" }))
keymap("n", "<leader>zf", "<cmd>TZFocus<CR>"      , F.KeymapArgs({ desc = "[Z]en [F]ocus" }))
keymap("n", "<leader>zm", "<cmd>TZMinimalist<CR>" , F.KeymapArgs({ desc = "[Z]en [M]inimalist" }))
keymap("n", "<leader>za", "<cmd>TZAtaraxis<CR>"   , F.KeymapArgs({ desc = "[Z]en [A]taraxis" }))

-------------------------------------------------------------------------------------------------------

-- ~  LSP keymaps

Keymaps = {}

Keymaps.LSP = function (_, bufnr)
  local lspbuf = vim.lsp.buf
  keymap("n", "<leader>rn", lspbuf.rename,      { buffer = bufnr, desc = "[R]e[n]ame" })
  keymap("n", "<leader>ca", lspbuf.code_action, { buffer = bufnr, desc = "[C]ode [A]ction" })
  keymap("n", "<leader>gd", lspbuf.definition,  { buffer = bufnr, desc = "[G]oto [d]efinition" })
  keymap("n", "<leader>gD", lspbuf.declaration, { buffer = bufnr, desc = "[G]oto [D]eclaration" })
  keymap("n", "<leader>gr", "<cmd>Telescope lsp_references<CR>", { buffer = bufnr, desc = "[G]oto [R]eferences" })
  keymap("n", "<leader>gi", lspbuf.implementation,  { buffer = bufnr, desc = "[G]oto [I]mplementation" })
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
