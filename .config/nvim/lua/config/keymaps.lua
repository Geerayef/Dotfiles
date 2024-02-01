vim.g.mapleader = " "

local keymap = F.Keymap
local bopt = { noremap = true, silent = true, desc = "" }

-- ~  General keymaps

keymap("x", "J", ":m '>+1<CR>gv=gv", bopt, "Move selected line[s] down")
keymap("x", "K", ":m '<-2<CR>gv=gv", bopt, "Move selected line[s] up")
keymap("n", "<C-d>", "10<C-d>", bopt, "Scroll down 10 lines")
keymap("n", "<C-u>", "10<C-u>", bopt, "Scroll up 10 lines")

-- Buffer
keymap("n", "<leader>bd", "<cmd>bdelete<CR>", bopt, "Delete current buffer")

-- Search
keymap("n", "<leader>nh", "<cmd>nohl<CR>", bopt, "Turn off highlights")
keymap("n", "n", "nzzzv", bopt, "Vertically center cursor after jumping to the next search result")
keymap("n", "N", "Nzzzv", bopt, "Vertically center cursor after jumping to the previous search result")

-- Copy/Yank/Paste
keymap("n", "x", '"_x', bopt, "Cut next character without saving to buffer")
keymap("x", "<leader>P", '"_dP', bopt, "Past from system clipboard")
keymap("n", "<leader>y", '"+y', bopt, "Yank to system clipboard")
keymap("n", "<leader>Y", '"+Y', bopt, "Yank to system clipboard")
keymap("v", "<leader>y", '"+y', bopt, "Yank to system clipboard")

-- Window Splitting
keymap("n", "<leader>spv", "<C-w>v", bopt, "[S]plit [V]ertically")
keymap("n", "<leader>sph", "<C-w>s", bopt, "[S]plit [H]orizontally")
keymap("n", "<leader>spe", "<C-w>=", bopt, "[S]plit [E]qualize sizes")
keymap("n", "<leader>spx", "<cmd>close<CR>", bopt, "[S]plit [X] close")

-- Tabs
keymap("n", "<leader>to", "<cmd>tabnew<CR>", bopt, "[T]ab [O]pen")
keymap("n", "<leader>tn", "<cmd>tabnext<CR>", bopt, "[T]ab [N]ext")
keymap("n", "<leader>tp", "<cmd>tabprevious<CR>", bopt, "[T]ab [P]revious")
keymap("n", "<leader>tmr", "<cmd>tabmove +1<CR>", bopt, "[T]ab [M]ove [L]eft")
keymap("n", "<leader>tml", "<cmd>tabmove -1<CR>", bopt, "[T]ab [M]ove [R]ight")

-- Terminal
keymap("t", "<Esc>", "<C-\\><C-n>", bopt, "Terminal mode: Escape")

-------------------------------------------------------------------------------------------------------

-- ~  Plugin keymaps

-- Arena
keymap("n", "<leader><space>", "<cmd>ArenaToggle<CR>", bopt, "[ ] Arena buffers")

-- Oil
keymap("n", "<leader>f", "<cmd>Oil<CR>", bopt, "Oil [F]ile Browser")
keymap("n", "<leader>of", "<cmd>Oil --float<CR>", bopt, "[O]il [F]loat")

-- Telescope
keymap("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", bopt, "[?] Recent files")
keymap("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", bopt, "[/] Search buffer")
keymap("n", "<leader>tb", "<cmd>Telescope builtin<CR>", bopt, "[T]elescope [B]uiltin")
keymap("n", "<leader>sf", "<cmd>Telescope fd<CR>", bopt, "Telescope [S]earch [F]iles")
keymap("n", "<leader>sh", "<cmd>Telescope help_tags<CR>", bopt, "Telescope [S]earch [H]elp")
keymap("n", "<leader>sw", "<cmd>Telescope grep_string<CR>", bopt, "Telescope [S]earch [W]ord")
keymap("n", "<leader>sg", "<cmd>Telescope live_grep<CR>", bopt, "Telescope [S]earch [G]rep")
keymap("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>", bopt, "Telescope [S]earch [D]iagnostics")

-- Gitsigns
keymap("n", "<leader>glb", "<cmd>Gitsigns toggle_current_line_blame<CR>", bopt, "[G]it [L]ine [B]lame")
-- Fugitive
keymap("n", "<leader>gs", "<cmd>Git<CR>", bopt, "[G]it [s]tatus")
keymap("n", "<leader>gab", "<cmd>Git add %<CR>", bopt, "[G]it [a]dd [b]uffer")
keymap("n", "<leader>gpl", "<cmd>Git pull<CR>", bopt, "[G]it [p]u[l]l")
keymap("n", "<leader>gps", "<cmd>Git push<CR>", bopt, "[G]it [p]u[s]h")

-- Zen
keymap("n", "<leader>zn", "<cmd>TZNarrow<CR>", bopt, "[Z]en [N]arrow")
keymap("v", "<leader>zn", "<cmd>'<,'>TZNarrow<CR>", bopt, "[Z]en [N]arrow selection")
keymap("n", "<leader>zf", "<cmd>TZFocus<CR>", bopt, "[Z]en [F]ocus")
keymap("n", "<leader>zm", "<cmd>TZMinimalist<CR>", bopt, "[Z]en [M]inimalist")
keymap("n", "<leader>za", "<cmd>TZAtaraxis<CR>", bopt, "[Z]en [A]taraxis")

-------------------------------------------------------------------------------------------------------

-- ~  LSP keymaps

Key = {}

function Key.LSP(_, bufnr)
  local lspbuf = vim.lsp.buf
  keymap("n", "<leader>rn", lspbuf.rename, { buffer = bufnr }, "[R]e[n]ame")
  keymap("n", "<leader>ca", lspbuf.code_action, { buffer = bufnr }, "[C]ode [A]ction")
  keymap("n", "<leader>gd", "<cmd>Telescope lsp_definitions<CR>", { buffer = bufnr }, "[G]oto [d]efinition")
  keymap("n", "<leader>gD", lspbuf.declaration, { buffer = bufnr }, "[G]oto [D]eclaration")
  keymap("n", "<leader>gr", "<cmd>Telescope lsp_references<CR>", { buffer = bufnr }, "[G]oto [R]eferences")
  keymap("n", "<leader>gi", "<cmd>Telescope lsp_implementations<CR>", { buffer = bufnr }, "[G]oto [I]mplementation")
  keymap("n", "<leader>D", lspbuf.type_definition, { buffer = bufnr }, "Type [D]efinition")
  keymap("n", "<leader>ds", "<cmd>Telescope lsp_document_symbols<CR>", { buffer = bufnr }, "[D]ocument [S]ymbols")
  keymap(
    "n",
    "<leader>ws",
    "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>",
    { buffer = bufnr },
    "[W]orkspace [S]ymbols"
  )
  keymap("n", "<leader>waf", lspbuf.add_workspace_folder, { buffer = bufnr }, "[W]orkspace [A]dd [F]older")
  keymap("n", "<leader>wrf", lspbuf.remove_workspace_folder, { buffer = bufnr }, "[W]orkspace [R]emove [F]older")
  keymap(
    "n",
    "<leader>wlf",
    "<cmd>lua = print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
    { buffer = bufnr },
    "[W]orkspace [L]ist [F]olders"
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
      swap_next = { ["<leader>snp"] = "@parameter.inner" },
      swap_previous = { ["<leader>spp"] = "@parameter.inner" },
    },
    lsp_interop = { ["<leader>pfd"] = "@function.outer", ["<leader>pcd"] = "@class.outer" },
  },
}

-- Java extensions provided by jdtls
function Key.JDTLS()
  keymap("n", "<leader>oi", '<cmd>lua require("jdtls").organize_imports<CR>', {}, "[O]rganize [I]mports")
  keymap("n", "<leader>ev", '<cmd>lua require("jdtls").extract_variable<CR>', {}, "[E]xtract [V]ariable")
  keymap("n", "<leader>ec", '<cmd>lua require("jdtls").extract_constant<CR>', {}, "[E]xtract [C]onstant")
  keymap("v", "<leader>em", '<cmd>lua require("jdtls").extract_method(true)<CR>', {}, "[E]xtract [M]ethod")
end

return Key
