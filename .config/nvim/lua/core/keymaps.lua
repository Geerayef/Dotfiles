vim.g.mapleader = " "
vim.g.maplocalleader = " "

local map = F.map
local bopt = { noremap = true, silent = true, desc = "" }

-- ~  General keymaps

-- Movement
map("x", "J", ":'<,'>m '>+1<CR>gv=gv", bopt, "Move selected line{s} down")
map("x", "K", ":'<,'>m '<-2<CR>gv=gv", bopt, "Move selected line{s} up")
map({ "n", "x" }, "<C-d>", "10j", bopt, "Scroll down 10 lines")
map({ "n", "x" }, "<C-u>", "10k", bopt, "Scroll up 10 lines")

-- Buffers
map("n", "<leader>bd", "<cmd>bdelete<CR>", bopt, "[b]uffer [d]elete")

-- Search
map("n", "<leader>nh", "<cmd>nohl<CR>", bopt, "[n]o [h]ighlights")
map("n", "n", "nzzzv", bopt, "Vertically center on next matching search")
map("n", "N", "Nzzzv", bopt, "Vertically center on previous matching search")

-- Copy/Yank/Paste
map("n", "x", "\"_x", bopt, "Cut rightward character without saving to buffer")
map("x", "<leader>P", "\"_dP", bopt, "Past from system clipboard")
map({ "n", "x" }, "<leader>y", "\"+y", bopt, "Yank to system clipboard")
map("n", "<leader>Y", "\"+Y", bopt, "Yank to system clipboard")

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

-- Commandline
map({ "l", "c" }, "<M-b>", "<C-Left>", bopt, "Word leftwards")
map({ "l", "c" }, "<M-w>", "<C-Right>", bopt, "Word rightwards")
map({ "l", "c" }, "<M-e>", "<End>", bopt, "End of line")
map({ "l", "c" }, "<M-a>", "<Home>", bopt, "Start of line")

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps

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

-- FzfLua
map("n", "<leader>z", "<cmd>FzfLua<CR>", bopt, "FzfLua")

-- Neogit
map("n", "<leader>G", "<cmd>Neogit<CR>", bopt, "Neo[G]it")

-- Gitsigns
map("n", "]h", function()
  if vim.wo.diff then
    vim.cmd.normal({ "]c", bang = true })
  else
    require("gitsigns").nav_hunk("next")
  end
end, bopt, "Next [h]unk")
map("n", "[h", function()
  if vim.wo.diff then
    vim.cmd.normal({ "[c", bang = true })
  else
    require("gitsigns").nav_hunk("prev")
  end
end, bopt, "Previous [h]unk")
map("n", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>", bopt, "Gitsigns [h]unk [s]tage")
map("n", "<leader>hS", "<cmd>Gitsigns stage_buffer<CR>", bopt, "Gitsigns [h]unk [S]tage buffer")
map("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>", bopt, "Gitsigns [h]unk [r]eset")
map("n", "<leader>hR", "<cmd>Gitsigns reset_buffer<CR>", bopt, "Gitsigns [h]unk [R]eset buffer")
map("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>", bopt, "Gitsigns [h]unk [u]ndo stage")
map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>", bopt, "Gitsigns [h]unk [p]review")
map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>", bopt, "Gitsigns [h]unk [d]iff")
map("n", "<leader>hbl", "<cmd>Gitsigns toggle_current_line_blame<CR>", bopt, "Gitsigns [h]unk [b]lame [l]ine")
map("n", "<leader>htd", "<cmd>Gitsigns toggle_deleted<CR>", bopt, "Gitsigns [h]unk [t]oggle [d]eleted")
map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", bopt, "Gitsigns TreeSitter textobjects [i]n [h]unk")

-------------------------------------------------------------------------------------------------------

-- ~  LSP keymaps

Key = {}

function Key.LSP(_, bufnr)
  local lspbuf = vim.lsp.buf
  map("n", "<leader>rn", lspbuf.rename, { buffer = bufnr }, "[r]e[n]ame")
  map("n", "<leader>ca", lspbuf.code_action, { buffer = bufnr }, "[c]ode [a]ction")
  map("n", "<leader>gD", lspbuf.declaration, { buffer = bufnr }, "[g]oto [D]eclaration")
  map("n", "<leader>D", lspbuf.type_definition, { buffer = bufnr }, "Type [D]efinition")
  map("n", "<leader>gd", "<cmd>Telescope lsp_definitions<CR>", { buffer = bufnr }, "[g]oto [d]efinition")
  map("n", "<leader>gr", "<cmd>Telescope lsp_references<CR>", { buffer = bufnr }, "[g]oto [r]eferences")
  map("n", "<leader>gi", "<cmd>Telescope lsp_implementations<CR>", { buffer = bufnr }, "[g]oto [i]mplementation")
  map("n", "<leader>ds", "<cmd>Telescope lsp_document_symbols<CR>", { buffer = bufnr }, "[d]ocument [s]ymbols")
  map("n", "<leader>ws", "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", { buffer = bufnr }, "[w]orkspace [s]ymbols")
  map("n", "<leader>waf", lspbuf.add_workspace_folder, { buffer = bufnr }, "[w]orkspace [a]dd [f]older")
  map("n", "<leader>wrf", lspbuf.remove_workspace_folder, { buffer = bufnr }, "[w]orkspace [r]emove [f]older")
  map("n", "K", lspbuf.hover, { buffer = bufnr }, "Hover Documentation")
  map("n", "<C-k>", lspbuf.signature_help, { buffer = bufnr }, "Signature Documentation")
end

-- TreeSitter
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

function Key.JDTLS()
  map("n", "<leader>oi", "<cmd>lua require(\"jdtls\").organize_imports<CR>", {}, "[o]rganize [i]mports")
  map("n", "<leader>ev", "<cmd>lua require(\"jdtls\").extract_variable<CR>", {}, "[e]xtract [v]ariable")
  map("n", "<leader>ec", "<cmd>lua require(\"jdtls\").extract_constant<CR>", {}, "[e]xtract [c]onstant")
  map("v", "<leader>em", "<cmd>lua require(\"jdtls\").extract_method(true)<CR>", {}, "[e]xtract [m]ethod")
end

return Key
