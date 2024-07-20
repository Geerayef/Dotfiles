vim.g.mapleader = " "
vim.g.maplocalleader = " "

local map = F.map
local bmap = F.bmap

-- ~ General --------------------------------------------------------------- ~ --

-- Movement
map("x", "J", ":'<,'>m '>+1<CR>gv=gv", "Move selection down")
map("x", "K", ":'<,'>m '<-2<CR>gv=gv", "Move selection up")
map({ "n", "x" }, "<C-d>", "10j", "Scroll down 10 lines")
map({ "n", "x" }, "<C-u>", "10k", "Scroll up 10 lines")

-- Buffers
map("n", "<leader>bd", "<cmd>bdelete<CR>", "[b]uffer [d]elete")
map("n", "<leader>cn", "<cmd>cnext<CR>", "[c] [n]ext")
map("n", "<leader>cp", "<cmd>cprevious<CR>", "[c] [p]revious")

-- Search
map("n", "<leader>nh", "<cmd>nohl<CR>", "[n]o [h]ighlights")
map("n", "n", "nzzzv", "Vertically center on next matching search")
map("n", "N", "Nzzzv", "Vertically center on previous matching search")

-- Copy/Yank/Paste
map("n", "x", "\"_x", "Cut rightward character without saving to buffer")
map("x", "<leader>P", "\"_dP", "Past from system clipboard")
map({ "n", "x" }, "<leader>y", "\"+y", "Yank to system clipboard")
map("n", "<leader>Y", "\"+Y", "Yank to system clipboard")

-- Tabs
map("n", "<leader>to", "<cmd>tabnew<CR>", "[t]ab [o]pen")
map("n", "<leader>tml", "<cmd>tabmove +1<CR>", "[t]ab [m]ove right")
map("n", "<leader>tmh", "<cmd>tabmove -1<CR>", "[t]ab [m]ove left")

-- Terminal
map("t", "<Esc>", "<C-\\><C-n>", "Terminal mode: Escape")

-- Diagnostic
map("n", "]d", vim.diagnostic.goto_next, "[d]iagnostic ] next")
map("n", "[d", vim.diagnostic.goto_prev, "[d]iagnostic [ previous")

-- Commandline
vim.api.nvim_set_keymap("c", "<C-a>", "<Home>", {})
vim.api.nvim_set_keymap("c", "<C-e>", "<End>", {})
vim.api.nvim_set_keymap("c", "<C-f>", "<Right>", {})
vim.api.nvim_set_keymap("c", "<C-b>", "<Left>", {})
vim.api.nvim_set_keymap("c", "<C-d>", "<Del>", {})
vim.api.nvim_set_keymap("c", "<M-f>", "<S-Right>", {})
vim.api.nvim_set_keymap("c", "<M-b>", "<S-Left>", {})

-- ~ Plugin ---------------------------------------------------------------- ~ --

-- stylua: ignore start
map("n", "<leader>L", "<cmd>Lazy<CR>", "[L]azy")
map("n", "<leader>f", "<cmd>Oil<CR>", "Oil [f]ile browser")
map("n", "<leader>of", "<cmd>Oil --float<CR>", "[o]il [f]loat")

-- Telescope
map("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", "[?] Recent files")
map("n", "<leader> ", "<cmd>Telescope buffers<CR>", "Telescope [s]earch [b]uffers")
map("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", "[/] Search buffer")
map("n", "<leader>tb", "<cmd>Telescope builtin<CR>", "[t]elescope [b]uiltin")
map("n", "<leader>sf", "<cmd>Telescope fd<CR>", "Telescope [s]earch [f]iles")
map("n", "<leader>sh", "<cmd>Telescope help_tags<CR>", "Telescope [s]earch [h]elp")
map("n", "<leader>sg", "<cmd>Telescope live_grep<CR>", "Telescope [s]earch [g]rep")
map("n", "<leader>sw", "<cmd>Telescope grep_string<CR>", "Telescope [s]earch [w]ord")
map("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>", "Telescope [s]earch [d]iagnostics")

-- Git
map("n", "<leader>G", "<cmd>Neogit<CR>", "Neo[G]it")
map("n", "]h", function()
  if vim.wo.diff then
    vim.cmd.normal({ "]c", bang = true })
  else
    require("gitsigns").nav_hunk("next")
  end
end, "Next [h]unk")
map("n", "[h", function()
  if vim.wo.diff then
    vim.cmd.normal({ "[c", bang = true })
  else
    require("gitsigns").nav_hunk("prev")
  end
end, "Previous [h]unk")
map("n", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>", "Gitsigns [h]unk [s]tage")
map("n", "<leader>hS", "<cmd>Gitsigns stage_buffer<CR>", "Gitsigns [h]unk [S]tage buffer")
map("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>", "Gitsigns [h]unk [r]eset")
map("n", "<leader>hR", "<cmd>Gitsigns reset_buffer<CR>", "Gitsigns [h]unk [R]eset buffer")
map("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>", "Gitsigns [h]unk [u]ndo stage")
map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>", "Gitsigns [h]unk [p]review")
map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>", "Gitsigns [h]unk [d]iff")
map("n", "<leader>hbl", "<cmd>Gitsigns toggle_current_line_blame<CR>", "Gitsigns [h]unk [b]lame [l]ine")
map("n", "<leader>htd", "<cmd>Gitsigns toggle_deleted<CR>", "Gitsigns [h]unk [t]oggle [d]eleted")
map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "Gitsigns TreeSitter textobjects [i]n [h]unk")

-- Obsidian & Markdown
map("n", "<leader>onn", "<cmd>ObsidianNew<CR>", "[o]bsidian [n]ote [n]ew")
map("n", "<leader>ont", "<cmd>ObsidianTemplate<CR>", "[o]bsidian [n]ote from [t]emplate")
map("n", "<leader>od", "<cmd>ObsidianToday<CR>", "[o]bsidian to[d]ay")
map("n", "<leader>om", "<cmd>ObsidianTomorrow<CR>", "[o]bsidian to[m]orrow")
map("n", "<leader>oy", "<cmd>ObsidianYesterday<CR>", "[o]bsidian [y]esterday")
map({ "n", "x" }, "<leader>oln", "<cmd>ObsidianLink<CR>", "[o]bsidian [l]ink [n]ew")
map("n", "<leader>olf", "<cmd>ObsidianFollowLink<CR>", "[o]bsidian [l]ink [f]ollow")
map("n", "<leader>olb", "<cmd>ObsidianBacklinks<CR>", "[o]bsidian [l]ink [b]aclinks")
map("n", "<leader>oss", "<cmd>ObsidianSearch<CR>", "[o]bsidian [s]earch")
map("n", "<leader>ost", "<cmd>ObsidianTags<CR>", "[o]bsidian [s]earch [t]ags")
map("n", "<leader>osd", "<cmd>ObsidianDailies<CR>", "[o]bsidian [s]earch [d]ailies")
map("n", "<leader>osl", "<cmd>ObsidianLinks<CR>", "[o]bsidian [s]earch [l]inks")
map("n", "<leader>ow", "<cmd>ObsidianWorkspace<CR>", "[o]obsidian [w]orkspace")
map("n", "<leader>oc", "<cmd>ObsidianToggleCheckbox<CR>", "[o]bsidian [c]heckbox")
map("n", "<leader>oq", "<cmd>ObsidianQuickSwitch<CR>", "[o]obsidian [q]uick switch")
map({ "n", "x" }, "<leader>oxn", "<cmd>ObsidianExtractNote<CR>", "[o]bsidian e[x]tract to new [n]ote and link to it")
map("n", "<leader>gp", "<cmd>Glow<CR>", "[g]low [p]review")

-- Colorizer
map("n", "<leader>ct", "<cmd>ColorizerToggle<CR>", "[c]olorizer [t]oggle")

-- ~ LSP ------------------------------------------------------------------- ~ --

Key = {}

function Key.LSP(_, buf)
  local lspbuf = vim.lsp.buf
  bmap("n", "K", lspbuf.hover, buf, "Hover")
  bmap("n", "<C-k>", lspbuf.signature_help, buf, "Signature")
  bmap("n", "<leader>rn", lspbuf.rename, buf, "[r]e[n]ame")
  bmap("n", "<leader>ca", lspbuf.code_action, buf, "[c]ode [a]ction")
  bmap("n", "<leader>gD", lspbuf.declaration, buf, "[g]oto [D]eclaration")
  bmap("n", "<leader>D", lspbuf.type_definition, buf, "Type [D]efinition")
  -- bmap("n", "<leader>gd", lspbuf.definition, buf, "[g]oto [d]efinition")
  -- bmap("n", "<leader>gr", lspbuf.references, buf, "[g]oto [r]eferences")
  -- bmap("n", "<leader>gi", lspbuf.implementation, buf, "[g]oto [i]mplementation")
  -- bmap("n", "<leader>ds", lspbuf.document_symbol, buf, "[d]ocument [s]ymbols")
  -- bmap("n", "<leader>ws", lspbuf.workspace_symbol, buf, "[w]orkspace [s]ymbols")
  bmap("n", "<leader>gd", "<cmd>Telescope lsp_definitions<CR>", buf, "[g]oto [d]efinition")
  bmap("n", "<leader>gr", "<cmd>Telescope lsp_references<CR>", buf, "[g]oto [r]eferences")
  bmap("n", "<leader>gi", "<cmd>Telescope lsp_implementations<CR>", buf, "[g]oto [i]mplementation")
  bmap("n", "<leader>ds", "<cmd>Telescope lsp_document_symbols<CR>", buf, "[d]ocument [s]ymbols")
  bmap("n", "<leader>ws", "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", buf, "[w]orkspace [s]ymbols")
  bmap("n", "<leader>waf", lspbuf.add_workspace_folder, buf, "[w]orkspace [a]dd [f]older")
  bmap("n", "<leader>wrf", lspbuf.remove_workspace_folder, buf, "[w]orkspace [r]emove [f]older")
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
      goto_next_start = { ["]f"] = "@function.outer", ["]["] = "@class.outer" },
      goto_next_end = { ["]F"] = "@function.outer", ["]]"] = "@class.outer" },
      goto_previous_start = { ["[f"] = "@function.outer", ["[["] = "@class.outer" },
      goto_previous_end = { ["]F"] = "@function.outer", ["[]"] = "@class.outer" },
    },
    swap = {
      next = { ["<M-C-L>"] = "@parameter.inner" },
      previous = { ["<M-C-H>"] = "@parameter.inner" },
    },
    lsp_interop = { ["<C-k>"] = "@function.outer", ["<C-K>"] = "@class.outer" },
  },
}

function Key.JDTLS()
  map("n", "<leader>oi", "<cmd>lua require(\"jdtls\").organize_imports<CR>", "[o]rganize [i]mports")
  map("n", "<leader>ev", "<cmd>lua require(\"jdtls\").extract_variable<CR>", "[e]xtract [v]ariable")
  map("n", "<leader>ec", "<cmd>lua require(\"jdtls\").extract_constant<CR>", "[e]xtract [c]onstant")
  map("v", "<leader>em", "<cmd>lua require(\"jdtls\").extract_method(true)<CR>", "[e]xtract [m]ethod")
end

-- stylua: ignore end

return Key
