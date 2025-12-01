vim.g.mapleader = " "
vim.g.maplocalleader = " "

---Map key sequence to action.
---Verbose. Buffer local.
---@param mode string|table # Mode{s}
---@param l string # Left side of mapping
---@param r string|function # Right side of mapping
---@param buf number # Buffer ID
---@param desc string # Mapping description
local function bmap(mode, l, r, buf, desc)
  vim.keymap.set(mode, l, r, { buffer = buf, desc = desc })
end

---Map key sequence to action.
---Wrapper for `vim.keymap.set`.
---@param mode string|table # Mode{s}
---@param l string # Left side of mapping
---@param r string|function # Right side of mapping
---@param desc string # Mapping description
---@param opts? vim.keymap.set.Opts # Options to vim.keymap.set
local function map(mode, l, r, desc, opts)
  local bo = { silent = true, desc = "" }
  bo.desc = #desc ~= 0 and desc or ("[" .. r .. "]")
  if opts then
    for k, v in pairs(opts) do
      bo[k] = v
    end
  end
  vim.keymap.set(mode, l, r, bo)
end

local cmd = vim.cmd
local input = vim.api.nvim_input

-- ~ General -------------------------------------------------------------- ~ --

-- Movement
map("x", "J", [[:'<,'>m '>+1<CR>gv=gv]], "Move selection down")
map("x", "K", [[:'<,'>m '<-2<CR>gv=gv]], "Move selection up")
map({ "n", "x" }, "<C-d>", "10j", "Scroll down 10 lines")
map({ "n", "x" }, "<C-u>", "10k", "Scroll up 10 lines")

-- Edit
map(
  "n",
  "<leader>yg",
  function() vim.fn.execute("normal yyPgcc") end,
  "Duplicate current line, commenting the original out",
  { remap = true }
)

-- Buffers
map("n", "<leader>bd", "<cmd>bd!<CR>", "[b]uffer [d]elete")

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
map("n", "<leader>tn", cmd.tabnew, "[t]ab [n]ew")
map("n", "<leader>to", cmd.tabonly, "[t]ab [o]nly")
map("n", "<leader>tml", "<cmd>tabmove +1<CR>", "[t]ab [m]ove [l] right")
map("n", "<leader>tmh", "<cmd>tabmove -1<CR>", "[t]ab [m]ove [h] left")

-- Diagnostic
map(
  "n",
  "]d",
  function() vim.diagnostic.jump({ count = 1, float = true }) end,
  "Next ] [d]iagnostic"
)
map(
  "n",
  "[d",
  function() vim.diagnostic.jump({ count = -1, float = true }) end,
  "Previous [ [d]iagnostic"
)

-- Commandline
-- vim.api.nvim_set_keymap("c", "<C-a>", "<Home>", {})
-- vim.api.nvim_set_keymap("c", "<C-e>", "<End>", {})
-- vim.api.nvim_set_keymap("c", "<C-f>", "<Right>", {})
-- vim.api.nvim_set_keymap("c", "<C-b>", "<Left>", {})
-- vim.api.nvim_set_keymap("c", "<C-j>", "<Down>", {})
-- vim.api.nvim_set_keymap("c", "<C-k>", "<Up>", {})
-- vim.api.nvim_set_keymap("c", "<C-d>", "<Del>", {})
-- vim.api.nvim_set_keymap("c", "<M-b>", "<S-Left>", {})
-- vim.api.nvim_set_keymap("c", "<M-f>", "<S-Right>", {})

-- Terminal
map("t", "<Esc>", "<C-\\><C-n>", "Terminal escape")

-- Navigation
map("n", "<leader>cd", function()
  cmd({ cmd = "cd", args = { "%:p:h" } })
  F.notify("INFO", string.format("CWD: %s", vim.fn.getcwd()))
end, "[c]hange to current working [d]irectory")

-- ~ Plugin --------------------------------------------------------------- ~ --

map("n", "<leader>L", cmd.Lazy, "[L]azy")
map("n", "<leader>f", cmd.Oil, "Oil [f]ile browser")
map("n", "<leader>of", "<cmd>Oil --float<CR>", "[o]il [f]loat")
map("n", "<leader>u", cmd.UndotreeToggle, "[u]ndo tree")
map(
  "n",
  "y<C-p>",
  function() vim.fn.setreg("+", require("jsonpath").get()) end,
  "[y]ank JSON[p]ath"
)

-- Flash
map({ "n", "x", "o" }, "<leader>j", function() require("flash").jump() end, "Flash [j]ump")

-- FZF Lua
map("i", "<C-x><C-f>", "<cmd>FzfLua complete_file<CR>", "FzfLua complete path")
map("i", "<C-x><C-l>", "<cmd>FzfLua complete_line<CR>", "FzfLua complete line")
map("n", "<leader>sf", "<cmd>FzfLua files<CR>", "FzfLua [s]earch [f]iles")
map("n", "<leader>sh", "<cmd>FzfLua helptags<CR>", "FzfLua [s]earch [h]elp")
map("n", "<leader>sg", "<cmd>FzfLua live_grep<CR>", "FzfLua [s]earch [g]rep")
map("n", "<leader>sw", "<cmd>FzfLua grep_cword<CR>", "FzfLua [s]earch [w]ord")
map("n", "<leader>sd", "<cmd>FzfLua diagnostics_document<CR>", "FzfLua [s]earch [d]iagnostics")
map(
  "n",
  "<leader>wd",
  "<cmd>FzfLua diagnostics_workspace<CR>",
  "FzfLua [w]orkspace [d]iagnostrics"
)
map("n", "<leader>ss", "<cmd>FzfLua spell_suggest<CR>", "FzFLua [s]earch [s]pell suggestions")
map("n", "<leader>?", "<cmd>FzfLua oldfiles<CR>", "[?] FzfLua recent files")
map("n", "<leader> ", "<cmd>FzfLua buffers<CR>", "[ ] FzfLua open buffers")
map("n", "<leader>/", "<cmd>FzfLua blines<CR>", "[/] FzfLua search buffer")

-- Git
-- Neogit
map("n", "<leader>G", "<cmd>Neogit<CR>", "Neo[G]it")
-- Fugitive
map("n", "<leader>Ga", function() input(":G add ") end, "Fu[G]itive [a]dd")
map("n", "<leader>Ga.", "<cmd>G add .<CR>", "Fu[G]itive [a]dd CWD")
map("n", "<leader>Gb", "<cmd>G branch<CR>", "Fu[G]itive [b]ranch")
map("n", "<leader>Gc", function() input(":G commit ") end, "Fu[G]itive [c]ommit")
map("n", "<leader>Gca", "<cmd>G commit --amend<CR>", "Fu[G]itive [c]ommit [a]mend")
map("n", "<leader>Gch", function() input(":G checkout ") end, "Fu[G]itive [c][h]eckout")
map("n", "<leader>Gf", "<cmd>G fetch<CR>", "Fu[G]itive [f]etch")
map("n", "<leader>Gpl", "<cmd>G pull --rebase<CR>", "Fu[G]itive [p]u[l]l")
map("n", "<leader>Gps", function() input(":G push ") end, "Fu[G]itive [p]u[s]h")
map("n", "<leader>Gr", function() input(":G restore ") end, "Fu[G]itive [r]estore")
map(
  "n",
  "<leader>Grs",
  function() input(":G restore --staged ") end,
  "Fu[G]itive [r]estore [s]taged"
)
map("n", "<leader>Gs", "<cmd>G status<CR>", "Fu[G]itive [s]tatus")
map("n", "<leader>Gst", "<cmd>G stash<CR>", "Fu[G]itive [s]tash")
map("n", "<leader>Gstp", "<cmd>G stash pop<CR>", "Fu[G]itive [s]tash [p]op")
-- Gitsigns
map("n", "]h", function()
  if vim.wo.diff then
    cmd.normal({ "]c", bang = true })
  else
    require("gitsigns").nav_hunk("next")
  end
end, "Gitsigns next [h]unk")
map("n", "[h", function()
  if vim.wo.diff then
    cmd.normal({ "[c", bang = true })
  else
    require("gitsigns").nav_hunk("prev")
  end
end, "Gitsigns previous [h]unk")
map("n", "<leader>hp", "<cmd>Gitsigns preview_hunk<CR>", "Gitsigns [h]unk [p]review")
map("n", "<leader>hs", "<cmd>Gitsigns stage_hunk<CR>", "Gitsigns [h]unk [s]tage")
map("n", "<leader>hS", "<cmd>Gitsigns undo_stage_hunk<CR>", "Gitsigns [h]unk un[S]tage")
map("n", "<leader>hr", "<cmd>Gitsigns reset_hunk<CR>", "Gitsigns [h]unk [r]eset")
map("n", "<leader>hd", "<cmd>Gitsigns diffthis<CR>", "Gitsigns [h]unk [d]iff")
map(
  "n",
  "<leader>htd",
  "<cmd>Gitsigns toggle_deleted<CR>",
  "Gitsigns [h]unk [t]oggle [d]eleted"
)
map(
  "n",
  "<leader>htlb",
  "<cmd>Gitsigns toggle_current_line_blame<CR>",
  "Gitsigns [h]unk [t]oggle [l]ine [b]lame"
)
-- Windows
if vim.fn.has("win32") == 1 or vim.fn.has("wsl") == 1 then
  map("n", "<leader>Wgs", function() input(":!git.exe status<CR>") end, "[W]indows [g]it [a]dd")
  map("n", "<leader>Wga", function() input(":!git.exe add ") end, "[W]indows [g]it [a]dd")
  map(
    "n",
    "<leader>Wga.",
    function() input(":!git.exe add .<CR>") end,
    "[W]indows [g]it [a]dd CWD"
  )
  map(
    "n",
    "<leader>Wgb",
    function() input(":!git.exe branch<CR>") end,
    "[W]indows [g]it [b]ranch"
  )
  map("n", "<leader>Wgc", function() input(":!git.exe commit ") end, "[W]indows [g]it [c]ommit")
  map(
    "n",
    "<leader>Wgca",
    function() input(":!git.exe commit --amend<CR>") end,
    "[W]indows [g]it [c]ommit [a]mend"
  )
  map(
    "n",
    "<leader>Wgch",
    function() input(":!git.exe checkout ") end,
    "[W]indows [g]it [c][h]eckout"
  )
  map(
    "n",
    "<leader>Wgf",
    function() input(":!git.exe fetch<CR>") end,
    "[W]indows [g]it [f]etch"
  )
  map(
    "n",
    "<leader>Wgpl",
    function() input(":!git.exe pull --rebase<CR>") end,
    "[W]indows [g]it [p]u[l]l"
  )
  map("n", "<leader>Wgps", function() input(":!git.exe push ") end, "[W]indows [g]it [p]u[s]h")
  map(
    "n",
    "<leader>Wgr",
    function() input(":!git.exe restore ") end,
    "[W]indows [g]it [r]estore"
  )
end

-- Markdown & Obsidian
map("n", "<leader>on", "<cmd>Obsidian new<CR>", "[o]bsidian [n]ew note")
map("n", "<leader>ot", "<cmd>Obsidian template<CR>", "[o]bsidian [t]emplate note")
map({ "n", "x" }, "<leader>oln", "<cmd>Obsidian link<CR>", "[o]bsidian [l]ink [n]ew")
map("n", "<leader>olf", "<cmd>Obsidian follow_link<CR>", "[o]bsidian [l]ink [f]ollow")
map("n", "<leader>olb", "<cmd>Obsidian backlinks<CR>", "[o]bsidian [l]ink [b]aclinks")
map("n", "<leader>oss", "<cmd>Obsidian search<CR>", "[o]bsidian [s]earch [s]")
map("n", "<leader>ost", "<cmd>Obsidian tags<CR>", "[o]bsidian [s]earch [t]ags")
map("n", "<leader>osd", "<cmd>Obsidian dailies<CR>", "[o]bsidian [s]earch [d]ailies")
map("n", "<leader>osl", "<cmd>Obsidian links<CR>", "[o]bsidian [s]earch [l]inks")
map("n", "<leader>od", "<cmd>Obsidian today<CR>", "[o]bsidian to[d]ay")
map("n", "<leader>ow", "<cmd>Obsidian workspace<CR>", "[o]bsidian [w]orkspace")
map("n", "<leader>oc", "<cmd>Obsidian toggle_checkbox<CR>", "[o]bsidian [c]heckbox")
map("n", "<leader>oq", "<cmd>Obsidian quick_switch<CR>", "[o]bsidian [q]uick switch")
map(
  { "n", "x" },
  "<leader>oxn",
  "<cmd>Obsidian extract_note<CR>",
  "[o]bsidian e[x]tract [n]ote"
)

-- Colorizer
map("n", "<leader>ct", cmd.ColorizerToggle, "[c]olorizer [t]oggle")

-- DAP
map("n", "<M-d>c", cmd.DapContinue, "DAP [c]ontinue")
map("n", "<M-d>o", cmd.DapStepOver, "DAP step [o]ver")
map("n", "<M-d>i", cmd.DapStepInto, "DAP step [i]nto")
map("n", "<M-d>u", cmd.DapStepOut, "DAP step o[u]t")
map("n", "<M-d>b", cmd.DapToggleBreakpoint, "DAP toggle [b]reakpoint")
map("n", "<M-d>B", cmd.DapClearBreakpoints, "DAP clear all [B]reakpoints")
map("n", "<M-d>t", cmd.DapTerminate, "DAP [t]erminate session")
map("n", "<M-d>S", function()
  local w = require("dap.ui.widgets")
  local s = w.sidebar(w.scopes, {}, "vsplit")
  return s.toggle()
end, "DAP [S]idebar")
map("n", "<M-d>F", function()
  local w = require("dap.ui.widgets")
  local b = w.sidebar(w.frames, { height = 10 }, "belowright split")
  return b.toggle()
end, "DAP [F]rames")
map("n", "<M-d>H", function() return require("dap.ui.widgets").hover() end, "DAP [H]over")

-- NoNeckPain
map("n", "<leader>Zen", function()
  require("twilight").toggle()
  require("no-neck-pain").toggle()
end, "NoNeckPain [Zen]")
map("n", "<leader>Zp", cmd.NoNeckPainScratchPad, "NoNeckPain [Z]en scratch [p]ad")

-- ~ LSP ------------------------------------------------------------------ ~ --

Key = {}
function Key.LSP(_, buf)
  local blsp = vim.lsp.buf
  bmap("n", "K", function() blsp.hover({ border = S.Border }) end, buf, "LSP hover")
  bmap(
    "n",
    "<C-k>",
    function() blsp.signature_help({ border = S.Border }) end,
    buf,
    "LSP signature"
  )
  bmap("n", "<leader>lrn", blsp.rename, buf, "[l]SP [r]e[n]ame")
  bmap("n", "<leader>lca", "<cmd>FzfLua lsp_code_actions<CR>", buf, "[l]SP [c]ode [a]ction")
  bmap(
    "n",
    "<leader>lgD",
    "<cmd>FzfLua lsp_declarations<CR>",
    buf,
    "[l]SP [g]o to [D]eclaration"
  )
  bmap("n", "<leader>lgd", "<cmd>FzfLua lsp_definitions<CR>", buf, "[l]SP [g]o to [d]efinition")
  bmap("n", "<leader>lgr", "<cmd>FzfLua lsp_references<CR>", buf, "[l]SP [g]o to [r]eferences")
  bmap(
    "n",
    "<leader>lgi",
    "<cmd>FzfLua lsp_implementations<CR>",
    buf,
    "[l]SP [g]o to [i]mplementation"
  )
  bmap(
    "n",
    "<leader>lgt",
    "<cmd>FzfLua lsp_type_definitions<CR>",
    buf,
    "[l]SP [g]o to [t]ype definition"
  )
  bmap(
    "n",
    "<leader>lsd",
    "<cmd>FzfLua lsp_document_symbols<CR>",
    buf,
    "[l]SP [s]ymbols [d]ocument"
  )
  bmap(
    "n",
    "<leader>lsw",
    "<cmd>FzfLua lsp_live_workspace_symbols<CR>",
    buf,
    "[l]SP [s]ymbols [w]orkspace"
  )
end

return Key
