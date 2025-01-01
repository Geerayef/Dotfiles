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
map({ "n", "x" }, "<Space>", "", "Disable moving cursor with space")

-- Edit
map("n", "<C-i>", "<C-i>", "Disambiguate from <tab>")
map("n", "<tab>", ">>", "Indent line using <tab>")
map("n", "<bs>", "<<", "Unindent line using <bs>")

-- Buffers
map("n", "<leader>bd", cmd.bd, "[b]uffer [d]elete")

-- Search
map("n", "<leader>nh", cmd.nohl, "[n]o [h]ighlights")
map("n", "n", "nzzzv", "Vertically center on next matching search")
map("n", "N", "Nzzzv", "Vertically center on previous matching search")

-- Yank/Paste
map("n", "x", '"_x', "Cut rightward character without saving to buffer")
map("x", "<leader>P", '"_dP', "Past from system clipboard")
map({ "n", "x" }, "<leader>y", '"+y', "Yank to system clipboard")
map("n", "<leader>Y", '"+Y', "Yank to system clipboard")

-- Tabs
map("n", "<leader>to", cmd.tabnew, "[t]ab [o]pen")
map("n", "<leader>tml", "<cmd>tabmove +1<CR>", "[t]ab [m]ove [l] right")
map("n", "<leader>tmh", "<cmd>tabmove -1<CR>", "[t]ab [m]ove [h] left")

-- Diagnostic
map("n", "]d", vim.diagnostic.goto_next, "Next ] [d]iagnostic")
map("n", "[d", vim.diagnostic.goto_prev, "Previous [ [d]iagnostic")

-- Commandline
map("n", "<CR>", ":<C-U>", "Enter key for cmdline")
vim.api.nvim_set_keymap("c", "<C-a>", "<Home>", {})
vim.api.nvim_set_keymap("c", "<C-e>", "<End>", {})
vim.api.nvim_set_keymap("c", "<C-f>", "<Right>", {})
vim.api.nvim_set_keymap("c", "<C-b>", "<Left>", {})
vim.api.nvim_set_keymap("c", "<C-n>", "<Down>", {})
vim.api.nvim_set_keymap("c", "<C-p>", "<Up>", {})
vim.api.nvim_set_keymap("c", "<C-d>", "<Del>", {})
vim.api.nvim_set_keymap("c", "<M-b>", "<S-Left>", {})
vim.api.nvim_set_keymap("c", "<M-f>", "<S-Right>", {})

-- Terminal
map("t", "<Esc>", "<C-\\><C-n>", "Terminal escape")

-- ~ Plugin ---------------------------------------------------------------- ~ --

map("n", "<leader>L", cmd.Lazy, "[L]azy")
map("n", "<leader>f", cmd.Oil, "Oil [f]ile browser")
map("n", "<leader>of", "<cmd>Oil --float<CR>", "[o]il [f]loat")
map("n", "<leader>u", vim.cmd.UndotreeToggle, "[u]ndo tree")

-- stylua: ignore start
-- Flash
map({"n", "x", "o"}, "<leader>j", function() require("flash").jump() end, "Flash [j]ump" )
map({"n", "x", "o"}, "<leader>t", function() require("flash").treesitter() end, "Flash [t]reesitter" )
map("c", "<C-s>", function() require("flash").toggle() end, "Toggle Flash Search")

-- Telescope
-- map("n", "<leader>sf", "<cmd>Telescope fd<CR>",          "Telescope [s]earch [f]iles")
-- map("n", "<leader>sh", "<cmd>Telescope help_tags<CR>",   "Telescope [s]earch [h]elp")
-- map("n", "<leader>sg", "<cmd>Telescope live_grep<CR>",   "Telescope [s]earch [g]rep")
-- map("n", "<leader>sw", "<cmd>Telescope grep_string<CR>", "Telescope [s]earch [w]ord")
-- map("n", "<leader>sd", "<cmd>Telescope diagnostics<CR>", "Telescope [s]earch [d]iagnostics")
-- map("n", "<leader>?", "<cmd>Telescope oldfiles<CR>",     "[?] Telescope recent files")
-- map("n", "<leader> ", "<cmd>Telescope buffers<CR>",      "[ ] Telescope buffers")
-- map("n", "<leader>/", "<cmd>Telescope current_buffer_fuzzy_find<CR>", "[/] Telescope search buffer")

-- FZF Lua
map("n", "<leader>sf", "<cmd>FzfLua files<CR>",                "FzfLua [s]earch [f]iles")
map("n", "<leader>sh", "<cmd>FzfLua helptags<CR>",             "FzfLua [s]earch [h]elp")
map("n", "<leader>sg", "<cmd>FzfLua live_grep<CR>",            "FzfLua [s]earch [g]rep")
map("n", "<leader>sw", "<cmd>FzfLua grep_cword<CR>",           "FzfLua [s]earch [w]ord")
map("n", "<leader>sd", "<cmd>FzfLua diagnostics_document<CR>", "FzfLua [s]earch [d]iagnostics")
map("n", "<leader>?", "<cmd>FzfLua oldfiles<CR>",              "[?] FzfLua recent files")
map("n", "<leader> ", "<cmd>FzfLua buffers<CR>",               "[ ] FzfLua open buffers")
map("n", "<leader>/", "<cmd>FzfLua grep_curbuf<CR>",           "[/] FzfLua search buffer")

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
map("n", "<leader>hu", "<cmd>Gitsigns undo_stage_hunk<CR>", "Gitsigns [h]unk stage [u]ndo")
map("n", "<leader>hS", "<cmd>Gitsigns stage_buffer<CR>",    "Gitsigns [h]unk [S]tage buffer")
map("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>",      "Gitsigns [h]unk [r]eset")
map("n", "<leader>hR", "<cmd>Gitsigns reset_buffer<CR>",    "Gitsigns [h]unk [R]eset buffer")
map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>",    "Gitsigns [h]unk [p]review")
map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>",        "Gitsigns [h]unk [d]iff")
map("n", "<leader>htd", "<cmd>Gitsigns toggle_deleted<CR>", "Gitsigns [h]unk [t]oggle [d]eleted")
map("n", "<leader>hb", "<cmd>Gitsigns toggle_current_line_blame<CR>", "Gitsigns [h]unk [b]lame line")
map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>",   "Gitsigns TreeSitter textobject [i]n [h]unk")

-- Markdown & Obsidian
map("n", "<leader>on", cmd.ObsidianNew,            "[o]bsidian [n]ew note")
map("n", "<leader>ot", cmd.ObsidianTemplate,       "[o]bsidian [t]emplate note")
map({ "n", "x" }, "<leader>oln", cmd.ObsidianLink, "[o]bsidian [l]ink [n]ew")
map("n", "<leader>olf", cmd.ObsidianFollowLink,    "[o]bsidian [l]ink [f]ollow")
map("n", "<leader>olb", cmd.ObsidianBacklinks,     "[o]bsidian [l]ink [b]aclinks")
map("n", "<leader>oss", cmd.ObsidianSearch,        "[o]bsidian [s]earch [s]")
map("n", "<leader>ost", cmd.ObsidianTags,          "[o]bsidian [s]earch [t]ags")
map("n", "<leader>osd", cmd.ObsidianDailies,       "[o]bsidian [s]earch [d]ailies")
map("n", "<leader>osl", cmd.ObsidianLinks,         "[o]bsidian [s]earch [l]inks")
map("n", "<leader>od", cmd.ObsidianToday,          "[o]bsidian to[d]ay")
map("n", "<leader>ow", cmd.ObsidianWorkspace,      "[o]bsidian [w]orkspace")
map("n", "<leader>oc", cmd.ObsidianToggleCheckbox, "[o]bsidian [c]heckbox")
map("n", "<leader>oq", cmd.ObsidianQuickSwitch,    "[o]bsidian [q]uick switch")
map({ "n", "x" }, "<leader>oxn", cmd.ObsidianExtractNote, "[o]bsidian e[x]tract [n]ote")

-- Colorizer
map("n", "<leader>ct", cmd.ColorizerToggle, "[c]olorizer [t]oggle")

-- DAP
map("n", "<M-d>c", cmd.DapContinue,         "DAP [c]ontinue")
map("n", "<M-d>o", cmd.DapStepOver,         "DAP step [o]ver")
map("n", "<M-d>i", cmd.DapStepInto,         "DAP step [i]nto")
map("n", "<M-d>u", cmd.DapStepOut,          "DAP step o[u]t")
map("n", "<M-d>b", cmd.DapToggleBreakpoint, "DAP toggle [b]reakpoint")
-- map("n", "<M-d>B", function() require("dap").clear_breakpoints() end, "DAP clear all breakpoints [B]")
map("n", "<M-d>B", cmd.DapClearBreakpoints, "DAP clear all [B]reakpoints")
map("n", "<M-d>t", cmd.DapTerminate, "DAP [t]erminate session")
map("n", "<leader>DS", function ()
  local w = require("dap.ui.widgets")
  local s = w.sidebar(w.scopes, {}, "vsplit")
  return s.toggle()
end, "[D]AP [S]idebar")
map("n", "<leader>DJ", function ()
  local w = require("dap.ui.widgets")
  local b = w.sidebar(w.frames, { height = 10 }, "belowright split")
  return b.toggle()
end, "[D]AP [F]rames")
map("n", "<leader>DH", function () return require("dap.ui.widgets").hover() end, "[D]AP [H]over")

-- ~ LSP ------------------------------------------------------------------- ~ --

Key = {}

function Key.LSP(_, buf)
  local blsp = vim.lsp.buf
  bmap("n", "K",           blsp.hover,                                   buf, "LSP hover")
  bmap("n", "<C-k>",       blsp.signature_help,                          buf, "LSP signature")
  bmap("n", "<leader>rn",  blsp.rename,                                  buf, "LSP [r]e[n]ame")
  bmap("n", "<leader>ca",  "<cmd>FzfLua lsp_code_actions<CR>",           buf, "LSP [c]ode [a]ction")
  bmap("n", "<leader>gD",  blsp.declaration,                             buf, "LSP [g]o to [D]eclaration")
  bmap("n", "<leader>gd",  "<cmd>FzfLua lsp_definitions<CR>",            buf, "LSP [g]o to [d]efinition")
  bmap("n", "<leader>gr",  "<cmd>FzfLua lsp_references<CR>",             buf, "LSP [g]o to [r]eferences")
  bmap("n", "<leader>gi",  "<cmd>FzfLua lsp_implementations<CR>",        buf, "LSP [g]o to [i]mplementation")
  bmap("n", "<leader>gt",  "<cmd>FzfLua lsp_type_definitions<CR>",       buf, "LSP [g]o to [t]ype definition")
  bmap("n", "<leader>ds",  "<cmd>FzfLua lsp_document_symbols<CR>",       buf, "LSP [d]ocument [s]ymbols")
  bmap("n", "<leader>ws",  "<cmd>FzfLua lsp_live_workspace_symbols<CR>", buf, "LSP [w]orkspace [s]ymbols")
  -- bmap("n", "<leader>wfa", blsp.add_workspace_folder,                          buf, "LSP [w]orkspace [f]older [a]dd")
  -- bmap("n", "<leader>wfr", blsp.remove_workspace_folder,                       buf, "LSP [w]orkspace [f]older [r]emove")
  bmap("n", "<leader>bac", LSP.buf_active_clients,                             buf, "LSP [b]uffer [a]ctive [c]lients")
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
      next = { ["<M-C-n>"] = "@parameter.inner", ["<S-M-C-n>"] = "@function.outer" },
      previous = { ["<M-C-p>"] = "@parameter.inner", ["<S-M-C-p>"] = "@function.outer" },
    },
    lsp_interop = { ["<C-k>"] = "@function.outer", ["<C-K>"] = "@class.outer" },
  },
}

function Key.JDTLS()
  map("n", "<leader>oi", "<cmd>lua require('jdtls').organize_imports<CR>", "[o]rganize [i]mports")
  map("n", "<leader>ev", "<cmd>lua require('jdtls').extract_variable<CR>", "[e]xtract [v]ariable")
  map("n", "<leader>ec", "<cmd>lua require('jdtls').extract_constant<CR>", "[e]xtract [c]onstant")
  map("v", "<leader>em", "<cmd>lua require('jdtls').extract_method(true)<CR>", "[e]xtract [m]ethod")
end
-- stylua: ignore end

return Key
