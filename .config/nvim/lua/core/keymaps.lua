vim.g.mapleader = " "

---@param mode string|table Mode[s]
---@param r string Right side of mapping
---@param l string|function Left side of mapping
---@param bo table Buffer options
---@param desc string Mapping description
local function map(mode, r, l, bo, desc)
  bo.desc = desc
  vim.keymap.set(mode, r, l, bo)
end

local bopt = { noremap = true, silent = true, desc = "" }

-- ~  General keymaps

map("x", "J", ":'<,'>m '>+1<CR>gv=gv", bopt, "Move selected line[s] down")
map("x", "K", ":'<,'>m '<-2<CR>gv=gv", bopt, "Move selected line[s] up")
map({ "n", "x" }, "<C-d>", "10j", bopt, "Scroll down 10 lines")
map({ "n", "x" }, "<C-u>", "10k", bopt, "Scroll up 10 lines")

-- Buffers
map("n", "<leader>bd", "<cmd>bdelete<CR>", bopt, "[b]uffer [d]elete")

-- Search
map("n", "<leader>nh", "<cmd>nohl<CR>", bopt, "[n]o [h]ighlights")
map("n", "n", "nzzzv", bopt, "Vertically center cursor after jumping to the next search result")
map("n", "N", "Nzzzv", bopt, "Vertically center cursor after jumping to the previous search result")

-- Copy/Yank/Paste
map("n", "x", "\"_x", bopt, "Cut next character without saving to buffer")
map("x", "<leader>P", "\"_dP", bopt, "Past from system clipboard")
map("n", "<leader>y", "\"+y", bopt, "Yank to system clipboard")
map("n", "<leader>Y", "\"+Y", bopt, "Yank to system clipboard")
map("v", "<leader>y", "\"+y", bopt, "Yank to system clipboard")

-- Tabs
map("n", "<leader>to", "<cmd>tabnew<CR>", bopt, "[t]ab [o]pen")
map("n", "<leader>tn", "<cmd>tabnext<CR>", bopt, "[t]ab [n]ext")
map("n", "<leader>tp", "<cmd>tabprevious<CR>", bopt, "[t]ab [p]revious")
map("n", "<leader>tmr", "<cmd>tabmove +1<CR>", bopt, "[t]ab [m]ove [l]eft")
map("n", "<leader>tml", "<cmd>tabmove -1<CR>", bopt, "[t]ab [m]ove [r]ight")

-- Terminal
map("t", "<Esc>", "<C-\\><C-n>", bopt, "Terminal mode: Escape")

-- Diagnostic
map("n", "<leader>dn", vim.diagnostic.goto_next, bopt, "[d]iagnostic [n]ext")
map("n", "<leader>dp", vim.diagnostic.goto_prev, bopt, "[d]iagnostic [p]revious")

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps

-- Arena
-- keymap("n", "<leader><space>", "<cmd>ArenaToggle<CR>", bopt, "[ ] Arena buffers")

-- Oil
map("n", "<leader>f", "<cmd>Oil<CR>", bopt, "Oil [f]ile browser")
map("n", "<leader>of", "<cmd>Oil --float<CR>", bopt, "[o]il [f]loat")

-- Telescope
map("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", bopt, "[?] Recent files")
map("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", bopt, "[/] Search buffer")
map("n", "<leader>tb", "<cmd>Telescope builtin<CR>", bopt, "[t]elescope [b]uiltin")
map("n", "<leader>sf", "<cmd>Telescope fd<CR>", bopt, "Telescope [s]earch [f]iles")
map("n", "<leader> ", "<cmd>Telescope buffers<CR>", bopt, "Telescope [s]earch [b]uffers")
map("n", "<leader>sh", "<cmd>Telescope help_tags<CR>", bopt, "Telescope [s]earch [h]elp")
map("n", "<leader>sg", "<cmd>Telescope live_grep<CR>", bopt, "Telescope [s]earch [g]rep")
map("n", "<leader>sw", "<cmd>Telescope grep_string<CR>", bopt, "Telescope [s]earch [w]ord")
map("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>", bopt, "Telescope [s]earch [d]iagnostics")

-- Gitsigns
map("n", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>", bopt, "[h]unk [s]tage")
map("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>", bopt, "[h]unk [r]eset")
map("n", "<leader>hS", "<cmd>Gitsigns stage_buffer<CR>", bopt, "[h]unk [S]tage buffer")
map("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>", bopt, "[h]unk [u]ndo stage")
map("n", "<leader>hR", "<cmd>Gitsigns reset_buffer<CR>", bopt, "[h]unk [R]eset buffer")
map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>", bopt, "[h]unk [p]review")
map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>", bopt, "[h]unk [d]iff")
map("n", "<leader>hbl", "<cmd>Gitsigns toggle_current_line_blame<CR>", bopt, "[h]unk [b]lame [l]ine")
map("n", "<leader>htd", "<cmd>Gitsigns toggle_deleted<CR>", bopt, "[h]unk [t]oggle [d]eleted")
map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", bopt, "Select [i]n [h]unk")

-- Neogit
map("n", "<leader>G", "<cmd>Neogit<CR>", bopt, "Neo[G]it")

-- Fugitive
-- keymap("n", "<leader>gs", "<cmd>Git<CR>", bopt, "[g]it [s]tatus")
-- keymap("n", "<leader>gab", "<cmd>Git add %<CR>", bopt, "[g]it [a]dd [b]uffer")
-- keymap("n", "<leader>gpl", "<cmd>Git pull<CR>", bopt, "[g]it [p]u[l]l")
-- keymap("n", "<leader>gps", "<cmd>Git push<CR>", bopt, "[g]it [p]u[s]h")

-- Conform
-- keymap(
--   "n",
--   "<leader>F",
--   function() require("conform").format({ async = true, lsp_fallback = true }) end,
--   bopt,
--   "[F]ormat current buffer"
-- )

-- Zen
map("n", "<leader>zn", "<cmd>TZNarrow<CR>", bopt, "[z]en [n]arrow")
map("v", "<leader>zn", "<cmd>'<,'>TZNarrow<CR>", bopt, "[z]en [n]arrow selection")
map("n", "<leader>zf", "<cmd>TZFocus<CR>", bopt, "[z]en [f]ocus")
map("n", "<leader>zm", "<cmd>TZMinimalist<CR>", bopt, "[z]en [m]inimalist")
map("n", "<leader>za", "<cmd>TZAtaraxis<CR>", bopt, "[z]en [a]taraxis")

-------------------------------------------------------------------------------------------------------

-- ~  LSP keymaps

Key = {}

function Key.LSP(_, bufnr)
  local lspbuf = vim.lsp.buf
  map("n", "<leader>rn", lspbuf.rename, { buffer = bufnr }, "[r]e[n]ame")
  map("n", "<leader>ca", lspbuf.code_action, { buffer = bufnr }, "[c]ode [a]ction")
  map("n", "<leader>gd", "<cmd>Telescope lsp_definitions<CR>", { buffer = bufnr }, "[g]oto [d]efinition")
  map("n", "<leader>gD", lspbuf.declaration, { buffer = bufnr }, "[g]oto [D]eclaration")
  map("n", "<leader>gr", "<cmd>Telescope lsp_references<CR>", { buffer = bufnr }, "[g]oto [r]eferences")
  map("n", "<leader>gi", "<cmd>Telescope lsp_implementations<CR>", { buffer = bufnr }, "[g]oto [i]mplementation")
  map("n", "<leader>D", lspbuf.type_definition, { buffer = bufnr }, "Type [D]efinition")
  map("n", "<leader>ds", "<cmd>Telescope lsp_document_symbols<CR>", { buffer = bufnr }, "[d]ocument [s]ymbols")
  map("n", "<leader>ws", "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", { buffer = bufnr }, "[w]orkspace [s]ymbols")
  map("n", "<leader>waf", lspbuf.add_workspace_folder, { buffer = bufnr }, "[w]orkspace [a]dd [f]older")
  map("n", "<leader>wrf", lspbuf.remove_workspace_folder, { buffer = bufnr }, "[w]orkspace [r]emove [f]older")
  map(
    "n",
    "<leader>wlf",
    "<cmd>lua = print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
    { buffer = bufnr },
    "[w]orkspace [l]ist [f]olders"
  )
  map("n", "K", lspbuf.hover, { buffer = bufnr }, "Hover Documentation")
  map("n", "<C-k>", lspbuf.signature_help, { buffer = bufnr }, "Signature Documentation")
end

Key.TS = {
  incremental_selection = { init_selection = "<C-space>", scope_incremental = "<C-space>", node_decremental = "<C-S><space>" },
  textobjects = {
    select = { ["fo"] = "@function.outer", ["fi"] = "@function.inner", ["co"] = "@class.outer", ["ci"] = "@class.inner" },
    move = {
      goto_next_start = { ["[f"] = "@function.outer", ["]["] = "@class.outer" },
      goto_next_end = { ["]f"] = "@function.outer", ["]]"] = "@class.outer" },
      goto_previous_start = { ["[F"] = "@function.outer", ["[["] = "@class.outer" },
      goto_previous_end = { ["]F"] = "@function.outer", ["[]"] = "@class.outer" },
    },
    swap = { next = { ["<M-C-L>"] = "@parameter.inner" }, previous = { ["<M-C-H>"] = "@parameter.inner" } },
    lsp_interop = { ["<C-k>"] = "@function.outer", ["<C-K>"] = "@class.outer" },
  },
}

-- Java extensions provided by JDTLS
function Key.JDTLS()
  map("n", "<leader>oi", "<cmd>lua require(\"jdtls\").organize_imports<CR>", {}, "[o]rganize [i]mports")
  map("n", "<leader>ev", "<cmd>lua require(\"jdtls\").extract_variable<CR>", {}, "[e]xtract [v]ariable")
  map("n", "<leader>ec", "<cmd>lua require(\"jdtls\").extract_constant<CR>", {}, "[e]xtract [c]onstant")
  map("v", "<leader>em", "<cmd>lua require(\"jdtls\").extract_method(true)<CR>", {}, "[e]xtract [m]ethod")
end

return Key
