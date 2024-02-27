vim.g.mapleader = " "

---@param mode string|table Mode[s]
---@param r string Right side of mapping
---@param l string|function Left side of mapping
---@param bo table Buffer options
---@param desc string Mapping description
local function keymap(mode, r, l, bo, desc)
  bo.desc = desc
  vim.keymap.set(mode, r, l, bo)
end

local bopt = { noremap = true, silent = true, desc = "" }

-- ~  General keymaps

keymap("x", "J", ":'<,'>m '>+1<CR>gv=gv", bopt, "Move selected line[s] down")
keymap("x", "K", ":'<,'>m '<-2<CR>gv=gv", bopt, "Move selected line[s] up")
keymap("n", "<C-d>", "10j", bopt, "Scroll down 10 lines")
keymap("n", "<C-u>", "10k", bopt, "Scroll up 10 lines")

-- Buffers
keymap("n", "<leader>bd", "<cmd>bdelete<CR>", bopt, "[b]uffer [d]elete")

-- Search
keymap("n", "<leader>nh", "<cmd>nohl<CR>", bopt, "[n]o [h]ighlights")
keymap("n", "n", "nzzzv", bopt, "Vertically center cursor after jumping to the next search result")
keymap("n", "N", "Nzzzv", bopt, "Vertically center cursor after jumping to the previous search result")

-- Copy/Yank/Paste
keymap("n", "x", "\"_x", bopt, "Cut next character without saving to buffer")
keymap("x", "<leader>P", "\"_dP", bopt, "Past from system clipboard")
keymap("n", "<leader>y", "\"+y", bopt, "Yank to system clipboard")
keymap("n", "<leader>Y", "\"+Y", bopt, "Yank to system clipboard")
keymap("v", "<leader>y", "\"+y", bopt, "Yank to system clipboard")

-- Tabs
keymap("n", "<leader>to", "<cmd>tabnew<CR>", bopt, "[t]ab [o]pen")
keymap("n", "<leader>tn", "<cmd>tabnext<CR>", bopt, "[t]ab [n]ext")
keymap("n", "<leader>tp", "<cmd>tabprevious<CR>", bopt, "[t]ab [p]revious")
keymap("n", "<leader>tmr", "<cmd>tabmove +1<CR>", bopt, "[t]ab [m]ove [l]eft")
keymap("n", "<leader>tml", "<cmd>tabmove -1<CR>", bopt, "[t]ab [m]ove [r]ight")

-- Terminal
keymap("t", "<Esc>", "<C-\\><C-n>", bopt, "Terminal mode: Escape")

-- Diagnostic
keymap("n", "<leader>dn", vim.diagnostic.goto_next, bopt, "[d]iagnostic [n]ext")
keymap("n", "<leader>dp", vim.diagnostic.goto_prev, bopt, "[d]iagnostic [p]revious")

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps

-- Arena
keymap("n", "<leader><space>", "<cmd>ArenaToggle<CR>", bopt, "[ ] Arena buffers")

-- Oil
keymap("n", "<leader>f", "<cmd>Oil<CR>", bopt, "Oil [f]ile browser")
keymap("n", "<leader>of", "<cmd>Oil --float<CR>", bopt, "[o]il [f]loat")

-- Telescope
keymap("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", bopt, "[?] Recent files")
keymap("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", bopt, "[/] Search buffer")
keymap("n", "<leader>tsb", "<cmd>Telescope builtin<CR>", bopt, "[t]ele[s]cope [b]uiltin")
keymap("n", "<leader>sf", "<cmd>Telescope fd<CR>", bopt, "Telescope [s]earch [f]iles")
keymap("n", "<leader>sb", "<cmd>Telescope buffers<CR>", bopt, "Telescope [s]earch [b]uffers")
keymap("n", "<leader>sh", "<cmd>Telescope help_tags<CR>", bopt, "Telescope [s]earch [h]elp")
keymap("n", "<leader>sg", "<cmd>Telescope live_grep<CR>", bopt, "Telescope [s]earch [g]rep")
keymap("n", "<leader>sw", "<cmd>Telescope grep_string<CR>", bopt, "Telescope [s]earch [w]ord")
keymap("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>", bopt, "Telescope [s]earch [d]iagnostics")

-- Gitsigns
keymap("n", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>", bopt, "[h]unk [s]tage")
keymap("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>", bopt, "[h]unk [r]eset")
keymap("n", "<leader>hS", "<cmd>Gitsigns stage_buffer<CR>", bopt, "[h]unk [S]tage buffer")
keymap("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>", bopt, "[h]unk [u]ndo stage")
keymap("n", "<leader>hR", "<cmd>Gitsigns reset_buffer<CR>", bopt, "[h]unk [R]eset buffer")
keymap("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>", bopt, "[h]unk [p]review")
keymap("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>", bopt, "[h]unk [d]iff")
keymap("n", "<leader>hbl", "<cmd>Gitsigns toggle_current_line_blame<CR>", bopt, "[h]unk [b]lame [l]ine")
keymap("n", "<leader>htd", "<cmd>Gitsigns toggle_deleted<CR>", bopt, "[h]unk [t]oggle [d]eleted")
keymap({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", bopt, "Select [i]n [h]unk")

-- Neogit
keymap("n", "<leader>G", "<cmd>Neogit<CR>", bopt, "Neo[G]it")

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
keymap("n", "<leader>zn", "<cmd>TZNarrow<CR>", bopt, "[z]en [n]arrow")
keymap("v", "<leader>zn", "<cmd>'<,'>TZNarrow<CR>", bopt, "[z]en [n]arrow selection")
keymap("n", "<leader>zf", "<cmd>TZFocus<CR>", bopt, "[z]en [f]ocus")
keymap("n", "<leader>zm", "<cmd>TZMinimalist<CR>", bopt, "[z]en [m]inimalist")
keymap("n", "<leader>za", "<cmd>TZAtaraxis<CR>", bopt, "[z]en [a]taraxis")

-------------------------------------------------------------------------------------------------------

-- ~  LSP keymaps

Key = {}

function Key.LSP(_, bufnr)
  local lspbuf = vim.lsp.buf
  keymap("n", "<leader>rn", lspbuf.rename, { buffer = bufnr }, "[r]e[n]ame")
  keymap("n", "<leader>ca", lspbuf.code_action, { buffer = bufnr }, "[c]ode [a]ction")
  keymap("n", "<leader>gd", "<cmd>Telescope lsp_definitions<CR>", { buffer = bufnr }, "[g]oto [d]efinition")
  keymap("n", "<leader>gD", lspbuf.declaration, { buffer = bufnr }, "[g]oto [D]eclaration")
  keymap("n", "<leader>gr", "<cmd>Telescope lsp_references<CR>", { buffer = bufnr }, "[g]oto [r]eferences")
  keymap("n", "<leader>gi", "<cmd>Telescope lsp_implementations<CR>", { buffer = bufnr }, "[g]oto [i]mplementation")
  keymap("n", "<leader>D", lspbuf.type_definition, { buffer = bufnr }, "Type [D]efinition")
  keymap("n", "<leader>ds", "<cmd>Telescope lsp_document_symbols<CR>", { buffer = bufnr }, "[d]ocument [s]ymbols")
  keymap("n", "<leader>ws", "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", { buffer = bufnr }, "[w]orkspace [s]ymbols")
  keymap("n", "<leader>waf", lspbuf.add_workspace_folder, { buffer = bufnr }, "[w]orkspace [a]dd [f]older")
  keymap("n", "<leader>wrf", lspbuf.remove_workspace_folder, { buffer = bufnr }, "[w]orkspace [r]emove [f]older")
  keymap(
    "n",
    "<leader>wlf",
    "<cmd>lua = print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
    { buffer = bufnr },
    "[w]orkspace [l]ist [f]olders"
  )
  keymap("n", "K", lspbuf.hover, { buffer = bufnr }, "Hover Documentation")
  keymap("n", "<C-k>", lspbuf.signature_help, { buffer = bufnr }, "Signature Documentation")
end

Key.TS = {
  incremental_selection = {
    init_selection = "<C-space>",
    scope_incremental = "<C-space>",
    node_decremental = "<C-S><space>",
  },
  textobjects = {
    select = {
      ["fo"] = "@function.outer",
      ["fi"] = "@function.inner",
      ["co"] = "@class.outer",
      ["ci"] = "@class.inner",
    },
    move = {
      goto_next_start = { ["[f"] = "@function.outer", ["]["] = "@class.outer" },
      goto_next_end = { ["]f"] = "@function.outer", ["]]"] = "@class.outer" },
      goto_previous_start = { ["[F"] = "@function.outer", ["[["] = "@class.outer" },
      goto_previous_end = { ["]F"] = "@function.outer", ["[]"] = "@class.outer" },
    },
    swap = {
      next = { ["<M-C-L>"] = "@parameter.inner" },
      previous = { ["<M-C-H>"] = "@parameter.inner" },
    },
    lsp_interop = { ["<C-k>"] = "@function.outer", ["<C-K>"] = "@class.outer" },
  },
}

-- Java extensions provided by jdtls
function Key.JDTLS()
  keymap("n", "<leader>oi", "<cmd>lua require(\"jdtls\").organize_imports<CR>", {}, "[o]rganize [i]mports")
  keymap("n", "<leader>ev", "<cmd>lua require(\"jdtls\").extract_variable<CR>", {}, "[e]xtract [v]ariable")
  keymap("n", "<leader>ec", "<cmd>lua require(\"jdtls\").extract_constant<CR>", {}, "[e]xtract [c]onstant")
  keymap("v", "<leader>em", "<cmd>lua require(\"jdtls\").extract_method(true)<CR>", {}, "[e]xtract [m]ethod")
end

return Key
