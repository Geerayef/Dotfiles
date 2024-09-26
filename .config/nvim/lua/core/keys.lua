vim.g.mapleader = " "
vim.g.maplocalleader = " "

local map = F.map
local bmap = F.bmap
local cmd = vim.cmd

-- ~ General --------------------------------------------------------------- ~ --

-- Movement
map("x", "J", ":'<,'>m '>+1<CR>gv=gv", "Move selection down")
map("x", "K", ":'<,'>m '<-2<CR>gv=gv", "Move selection up")
map({ "n", "x" }, "<C-d>", "10j", "Scroll down 10 lines")
map({ "n", "x" }, "<C-u>", "10k", "Scroll up 10 lines")

-- Buffers
map("n", "<leader>bd", cmd.bd, "[b]uffer [d]elete")

-- Search
map("n", "<leader>nh", cmd.nohl, "[n]o [h]ighlights")
map("n", "n", "nzzzv", "Vertically center on next matching search")
map("n", "N", "Nzzzv", "Vertically center on previous matching search")

-- Yank/Paste
map("n", "x", "\"_x", "Cut rightward character without saving to buffer")
map("x", "<leader>P", "\"_dP", "Past from system clipboard")
map({ "n", "x" }, "<leader>y", "\"+y", "Yank to system clipboard")
map("n", "<leader>Y", "\"+Y", "Yank to system clipboard")

-- Tabs
map("n", "<leader>to", cmd.tabnew, "[t]ab [o]pen")
map("n", "<leader>tml", "<cmd>tabmove +1<CR>", "[t]ab [m]ove [l] right")
map("n", "<leader>tmh", "<cmd>tabmove -1<CR>", "[t]ab [m]ove [h] left")

-- Terminal
map("t", "<Esc>", "<C-\\><C-n>", "Terminal escape")

-- Diagnostic
map("n", "]d", vim.diagnostic.goto_next, "Next ] [d]iagnostic")
map("n", "[d", vim.diagnostic.goto_prev, "Previous [ [d]iagnostic")

-- Commandline
local key = vim.api.nvim_set_keymap
key("c", "<C-a>", "<Home>", {})
key("c", "<C-e>", "<End>", {})
key("c", "<C-f>", "<Right>", {})
key("c", "<C-b>", "<Left>", {})
key("c", "<C-d>", "<Del>", {})
key("c", "<M-f>", "<S-Right>", {})
key("c", "<M-b>", "<S-Left>", {})

-- ~ Plugin ---------------------------------------------------------------- ~ --

map("n", "<leader>L", cmd.Lazy, "[L]azy")
map("n", "<leader>f", cmd.Oil, "Oil [f]ile browser")
map("n", "<leader>of", "<cmd>Oil --float<CR>", "[o]il [f]loat")
map("n", "<leader>u", vim.cmd.UndotreeToggle, "[u]ndo tree")

-- stylua: ignore start
-- Telescope
map("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", "[?] Telescope recent files")
map("n", "<leader> ", "<cmd>Telescope buffers<CR>",  "[ ] Telescope buffers")
map("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", "[/] Telescope search buffer")
map("n", "<leader>tb", "<cmd>Telescope builtin<CR>",     "[t]elescope [b]uiltin")
map("n", "<leader>sf", "<cmd>Telescope fd<CR>",          "Telescope [s]earch [f]iles")
map("n", "<leader>sh", "<cmd>Telescope help_tags<CR>",   "Telescope [s]earch [h]elp")
map("n", "<leader>sg", "<cmd>Telescope live_grep<CR>",   "Telescope [s]earch [g]rep")
map("n", "<leader>sw", "<cmd>Telescope grep_string<CR>", "Telescope [s]earch [w]ord")
map("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>", "Telescope [s]earch [d]iagnostics")

-- Git
map("n", "<leader>G", "<cmd>Neogit<CR>", "Neo[G]it")
map("n", "]h", function()
  if vim.wo.diff then
    cmd.normal({ "]c", bang = true })
  else
    require("gitsigns").nav_hunk("next")
  end
end, "Next [h]unk")
map("n", "[h", function()
  if vim.wo.diff then
    cmd.normal({ "[c", bang = true })
  else
    require("gitsigns").nav_hunk("prev")
  end
end, "Previous [h]unk")
map("n", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>",      "Gitsigns [h]unk [s]tage")
map("n", "<leader>hS", "<cmd>Gitsigns stage_buffer<CR>",    "Gitsigns [h]unk [S]tage buffer")
map("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>",      "Gitsigns [h]unk [r]eset")
map("n", "<leader>hR", "<cmd>Gitsigns reset_buffer<CR>",    "Gitsigns [h]unk [R]eset buffer")
map("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>", "Gitsigns [h]unk [u]ndo stage")
map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>",    "Gitsigns [h]unk [p]review")
map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>",        "Gitsigns [h]unk [d]iff")
map("n", "<leader>htd", "<cmd>Gitsigns toggle_deleted<CR>", "Gitsigns [h]unk [t]oggle [d]eleted")
map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>",   "Gitsigns TreeSitter textobject [i]n [h]unk")
map("n", "<leader>hb", "<cmd>Gitsigns toggle_current_line_blame<CR>", "Gitsigns [h]unk [b]lame line")

-- Obsidian & Markdown
map("n", "<leader>onn", cmd.ObsidianNew,           "[o]bsidian [n]ote [n]ew")
map("n", "<leader>ont", cmd.ObsidianTemplate,      "[o]bsidian [n]ote from [t]emplate")
map({ "n", "x" }, "<leader>oln", cmd.ObsidianLink, "[o]bsidian [l]ink [n]ew")
map("n", "<leader>olf", cmd.ObsidianFollowLink,    "[o]bsidian [l]ink [f]ollow")
map("n", "<leader>olb", cmd.ObsidianBacklinks,     "[o]bsidian [l]ink [b]aclinks")
map("n", "<leader>oss", cmd.ObsidianSearch,        "[o]bsidian [s]earch [s]")
map("n", "<leader>ost", cmd.ObsidianTags,          "[o]bsidian [s]earch [t]ags")
map("n", "<leader>osd", cmd.ObsidianDailies,       "[o]bsidian [s]earch [d]ailies")
map("n", "<leader>osl", cmd.ObsidianLinks,         "[o]bsidian [s]earch [l]inks")
map("n", "<leader>od",  cmd.ObsidianToday,         "[o]bsidian to[d]ay")
map("n", "<leader>ow", cmd.ObsidianWorkspace,      "[o]bsidian [w]orkspace")
map("n", "<leader>oc", cmd.ObsidianToggleCheckbox, "[o]bsidian [c]heckbox")
map("n", "<leader>oq", cmd.ObsidianQuickSwitch,    "[o]bsidian [q]uick switch")
map({ "n", "x" }, "<leader>oxn", cmd.ObsidianExtractNote, "[o]bsidian e[x]tract [n]ote")
map("n", "<leader>gp", cmd.Glow, "[g]low [p]review")

-- Colorizer
map("n", "<leader>ct", cmd.ColorizerToggle, "[c]olorizer [t]oggle")

-- ~ LSP ------------------------------------------------------------------- ~ --

Key = {}

function Key.LSP(_, buf)
  local blsp = vim.lsp.buf
  bmap("n", "K", blsp.hover, buf, "Hover")
  bmap("n", "<C-k>", blsp.signature_help, buf, "Signature")
  bmap("n", "<leader>rn", blsp.rename, buf, "[r]e[n]ame")
  bmap("n", "<leader>ca", blsp.code_action, buf, "[c]ode [a]ction")
  bmap("n", "<leader>gD", blsp.declaration, buf, "[g]oto [D]eclaration")
  bmap("n", "<leader>D", blsp.type_definition, buf, "Type [D]efinition")
  bmap("n", "<leader>gd", "<cmd>Telescope lsp_definitions<CR>", buf, "[g]oto [d]efinition")
  bmap("n", "<leader>gr", "<cmd>Telescope lsp_references<CR>", buf, "[g]oto [r]eferences")
  bmap("n", "<leader>gi", "<cmd>Telescope lsp_implementations<CR>", buf, "[g]oto [i]mplementation")
  bmap("n", "<leader>ds", "<cmd>Telescope lsp_document_symbols<CR>", buf, "[d]ocument [s]ymbols")
  bmap("n", "<leader>ws", "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>", buf, "[w]orkspace [s]ymbols")
  bmap("n", "<leader>waf", blsp.add_workspace_folder, buf, "[w]orkspace [a]dd [f]older")
  bmap("n", "<leader>wrf", blsp.remove_workspace_folder, buf, "[w]orkspace [r]emove [f]older")
  bmap("n", "<leader>bac", LSP.buf_active_clients, buf, "[b]uffer [a]ctive [c]lients")
end

Key.TS = {
  incremental_selection = {
    init_selection = "<C-space>",
    scope_incremental = "<C-space>",
    node_decremental = "<C-S><space>",
  },
  textobjects = {
    select = {
      ["of"] = "@function.outer",
      ["if"] = "@function.inner",
      ["co"] = "@class.outer",
      ["ci"] = "@class.inner",
    },
    move = {
      goto_next_start = { ["]f"] = "@function.outer", ["]["] = "@class.outer" },
      goto_next_end = { ["]F"] = "@function.outer", ["]]"] = "@class.outer" },
      goto_previous_start = { ["[f"] = "@function.outer", ["[["] = "@class.outer" },
      goto_previous_end = { ["[F"] = "@function.outer", ["[]"] = "@class.outer" },
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
