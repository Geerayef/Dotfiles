vim.g.mapleader = " "
vim.g.maplocalleader = " "

local map = F.map
local bos = { noremap = true, silent = true, desc = "" }
local bov = { noremap = true, silent = false, desc = "" }

-- ~  General keymaps

-- Movement
map("x", "J", ":'<,'>m '>+1<CR>gv=gv", bos, "Move selected line{s} down")
map("x", "K", ":'<,'>m '<-2<CR>gv=gv", bos, "Move selected line{s} up")
map({ "n", "x" }, "<C-d>", "10j", bos, "Scroll down 10 lines")
map({ "n", "x" }, "<C-u>", "10k", bos, "Scroll up 10 lines")

-- Buffers
map("n", "<leader>bd", "<cmd>bdelete<CR>", bos, "[b]uffer [d]elete")

-- Search
map("n", "<leader>nh", "<cmd>nohl<CR>", bos, "[n]o [h]ighlights")
map("n", "n", "nzzzv", bos, "Vertically center on next matching search")
map("n", "N", "Nzzzv", bos, "Vertically center on previous matching search")

-- Copy/Yank/Paste
map("n", "x", "\"_x", bos, "Cut rightward character without saving to buffer")
map("x", "<leader>P", "\"_dP", bos, "Past from system clipboard")
map({ "n", "x" }, "<leader>y", "\"+y", bos, "Yank to system clipboard")
map("n", "<leader>Y", "\"+Y", bos, "Yank to system clipboard")

-- Tabs
map("n", "<leader>to", "<cmd>tabnew<CR>", bos, "[t]ab [o]pen")
map("n", "<leader>tn", "<cmd>tabnext<CR>", bos, "[t]ab [n]ext")
map("n", "<leader>tp", "<cmd>tabprevious<CR>", bos, "[t]ab [p]revious")
map("n", "<leader>tmr", "<cmd>tabmove +1<CR>", bos, "[t]ab [m]ove [l]eft")
map("n", "<leader>tml", "<cmd>tabmove -1<CR>", bos, "[t]ab [m]ove [r]ight")

-- Terminal
map("t", "<Esc>", "<C-\\><C-n>", bos, "Terminal mode: Escape")

-- Diagnostic
map("n", "<leader>dn", vim.diagnostic.goto_next, bos, "[d]iagnostic [n]ext")
map("n", "<leader>dp", vim.diagnostic.goto_prev, bos, "[d]iagnostic [p]revious")

-- ~ Plugin ---------------------------------------------------------------- ~ --

-- Lazy
map("n", "<leader>L", "<cmd>Lazy<CR>", bos, "[L]azy")

-- Oil
map("n", "<leader>f", "<cmd>Oil<CR>", bos, "Oil [f]ile browser")
map("n", "<leader>of", "<cmd>Oil --float<CR>", bos, "[o]il [f]loat")

-- Telescope
map("n", "<leader>?", "<cmd>Telescope oldfiles<CR>", bos, "[?] Recent files")
map(
  "n",
  "<leader>/",
  "<cmd>Telescope current_buffer_fuzzy_find<CR>",
  bos,
  "[/] Search buffer"
)
map(
  "n",
  "<leader>tb",
  "<cmd>Telescope builtin<CR>",
  bos,
  "[t]elescope [b]uiltin"
)
map(
  "n",
  "<leader>sf",
  "<cmd>Telescope fd<CR>",
  bos,
  "Telescope [s]earch [f]iles"
)
map(
  "n",
  "<leader> ",
  "<cmd>Telescope buffers<CR>",
  bos,
  "Telescope [s]earch [b]uffers"
)
map(
  "n",
  "<leader>sh",
  "<cmd>Telescope help_tags<CR>",
  bos,
  "Telescope [s]earch [h]elp"
)
map(
  "n",
  "<leader>sg",
  "<cmd>Telescope live_grep<CR>",
  bos,
  "Telescope [s]earch [g]rep"
)
map(
  "n",
  "<leader>sw",
  "<cmd>Telescope grep_string<CR>",
  bos,
  "Telescope [s]earch [w]ord"
)
map(
  "n",
  "<leader>sd",
  "<cmd>Telescope diagnostics<CR>",
  bos,
  "Telescope [s]earch [d]iagnostics"
)

-- Neogit
map("n", "<leader>G", "<cmd>Neogit<CR>", bos, "Neo[G]it")

-- Gitsigns
map("n", "]h", function()
  if vim.wo.diff then
    vim.cmd.normal({ "]c", bang = true })
  else
    require("gitsigns").nav_hunk("next")
  end
end, bos, "Next [h]unk")
map("n", "[h", function()
  if vim.wo.diff then
    vim.cmd.normal({ "[c", bang = true })
  else
    require("gitsigns").nav_hunk("prev")
  end
end, bos, "Previous [h]unk")
map(
  "n",
  "<leader>hs",
  "<cmd>Gitsigns stage_hunk<CR>",
  bos,
  "Gitsigns [h]unk [s]tage"
)
map(
  "n",
  "<leader>hS",
  "<cmd>Gitsigns stage_buffer<CR>",
  bos,
  "Gitsigns [h]unk [S]tage buffer"
)
map(
  "n",
  "<leader>hr",
  "<cmd>Gitsigns reset_hunk<CR>",
  bos,
  "Gitsigns [h]unk [r]eset"
)
map(
  "n",
  "<leader>hR",
  "<cmd>Gitsigns reset_buffer<CR>",
  bos,
  "Gitsigns [h]unk [R]eset buffer"
)
map(
  "n",
  "<leader>hu",
  "<cmd>Gitsigns undo_stage_hunk<CR>",
  bos,
  "Gitsigns [h]unk [u]ndo stage"
)
map(
  "n",
  "<leader>hp",
  "<cmd>Gitsigns preview_hunk<CR>",
  bos,
  "Gitsigns [h]unk [p]review"
)
map(
  "n",
  "<leader>hd",
  "<cmd>Gitsigns diffthis<CR>",
  bos,
  "Gitsigns [h]unk [d]iff"
)
map(
  "n",
  "<leader>hbl",
  "<cmd>Gitsigns toggle_current_line_blame<CR>",
  bos,
  "Gitsigns [h]unk [b]lame [l]ine"
)
map(
  "n",
  "<leader>htd",
  "<cmd>Gitsigns toggle_deleted<CR>",
  bos,
  "Gitsigns [h]unk [t]oggle [d]eleted"
)
map(
  { "o", "x" },
  "ih",
  ":<C-U>Gitsigns select_hunk<CR>",
  bos,
  "Gitsigns TreeSitter textobjects [i]n [h]unk"
)

-- Obsidian & Markdown
map("n", "<leader>onn", ":ObsidianNew ", bov, "[o]bsidian [n]ote [n]ew")
map(
  "n",
  "<leader>ond",
  "<cmd>ObsidianToday<CR>",
  bos,
  "[o]bsidian [n]ote to[d]ay"
)
map(
  "n",
  "<leader>onm",
  "<cmd>ObsidianTomorrow<CR>",
  bos,
  "[o]bsidian [n]ote to[m]orrow"
)
map(
  "n",
  "<leader>ony",
  "<cmd>ObsidianYesterday<CR>",
  bos,
  "[o]bsidian [n]ote [y]esterday"
)
map(
  "n",
  "<leader>ont",
  "<cmd>ObsidianTemplate<CR>",
  bos,
  "[o]bsidian [n]ote from [t]emplate"
)
map(
  "n",
  "<leader>oct",
  "<cmd>ObsidianToggleCheckbox<CR>",
  bos,
  "[o]bsidian [c]heckbox [t]oggle"
)
map(
  { "n", "x" },
  "<leader>oln",
  "<cmd>ObsidianLink<CR>",
  bos,
  "[o]bsidian [l]ink [n]ew"
)
map(
  "n",
  "<leader>olf",
  "<cmd>ObsidianFollowLink<CR>",
  bos,
  "[o]bsidian [l]ink [f]ollow"
)
map(
  "n",
  "<leader>olb",
  "<cmd>ObsidianBacklinks<CR>",
  bos,
  "[o]bsidian [l]ink [b]aclinks"
)
map(
  "n",
  "<leader>ost",
  "<cmd>ObsidianTags<CR>",
  bos,
  "[o]bsidian [s]earch [t]ags"
)
map(
  "n",
  "<leader>osd",
  "<cmd>ObsidianDailies<CR>",
  bos,
  "[o]bsidian [s]earch [d]ailies"
)
map(
  "n",
  "<leader>osl",
  "<cmd>ObsidianLinks<CR>",
  bos,
  "[o]bsidian [s]earch [l]inks"
)
map(
  { "n", "x" },
  "<leader>oxn",
  "<cmd>ObsidianExtractNote<CR>",
  bos,
  "[o]bsidian e[x]tract to new [n]ote and link to it"
)
-- Preview
map("n", "<leader>mp", "<cmd>Preview<CR>", bos, "[m]arkdown [p]review")
map("n", "<leader>gp", "<cmd>Glow<CR>", bos, "[g]low [p]review")

-- Misc
map("n", "<leader>ct", "<cmd>ColorizerToggle<CR>", bos, "[c]olorizer [t]oggle")

-- ~ LSP ------------------------------------------------------------------- ~ --

Key = {}

function Key.LSP(_, bufnr)
  local lspbuf = vim.lsp.buf
  map("n", "<leader>rn", lspbuf.rename, { buffer = bufnr }, "[r]e[n]ame")
  map(
    "n",
    "<leader>ca",
    lspbuf.code_action,
    { buffer = bufnr },
    "[c]ode [a]ction"
  )
  map(
    "n",
    "<leader>gD",
    lspbuf.declaration,
    { buffer = bufnr },
    "[g]oto [D]eclaration"
  )
  map(
    "n",
    "<leader>D",
    lspbuf.type_definition,
    { buffer = bufnr },
    "Type [D]efinition"
  )
  map(
    "n",
    "<leader>gd",
    "<cmd>Telescope lsp_definitions<CR>",
    { buffer = bufnr },
    "[g]oto [d]efinition"
  )
  map(
    "n",
    "<leader>gr",
    "<cmd>Telescope lsp_references<CR>",
    { buffer = bufnr },
    "[g]oto [r]eferences"
  )
  map(
    "n",
    "<leader>gi",
    "<cmd>Telescope lsp_implementations<CR>",
    { buffer = bufnr },
    "[g]oto [i]mplementation"
  )
  map(
    "n",
    "<leader>ds",
    "<cmd>Telescope lsp_document_symbols<CR>",
    { buffer = bufnr },
    "[d]ocument [s]ymbols"
  )
  map(
    "n",
    "<leader>ws",
    "<cmd>Telescope lsp_dynamic_workspace_symbols<CR>",
    { buffer = bufnr },
    "[w]orkspace [s]ymbols"
  )
  map(
    "n",
    "<leader>waf",
    lspbuf.add_workspace_folder,
    { buffer = bufnr },
    "[w]orkspace [a]dd [f]older"
  )
  map(
    "n",
    "<leader>wrf",
    lspbuf.remove_workspace_folder,
    { buffer = bufnr },
    "[w]orkspace [r]emove [f]older"
  )
  map("n", "K", lspbuf.hover, { buffer = bufnr }, "Hover Documentation")
  map(
    "n",
    "<C-k>",
    lspbuf.signature_help,
    { buffer = bufnr },
    "Signature Documentation"
  )
end

-- TreeSitter
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
      goto_previous_start = {
        ["[F"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["]F"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
    swap = {
      next = { ["<M-C-L>"] = "@parameter.inner" },
      previous = { ["<M-C-H>"] = "@parameter.inner" },
    },
    lsp_interop = { ["<C-k>"] = "@function.outer", ["<C-K>"] = "@class.outer" },
  },
}

function Key.JDTLS()
  map(
    "n",
    "<leader>oi",
    "<cmd>lua require(\"jdtls\").organize_imports<CR>",
    {},
    "[o]rganize [i]mports"
  )
  map(
    "n",
    "<leader>ev",
    "<cmd>lua require(\"jdtls\").extract_variable<CR>",
    {},
    "[e]xtract [v]ariable"
  )
  map(
    "n",
    "<leader>ec",
    "<cmd>lua require(\"jdtls\").extract_constant<CR>",
    {},
    "[e]xtract [c]onstant"
  )
  map(
    "v",
    "<leader>em",
    "<cmd>lua require(\"jdtls\").extract_method(true)<CR>",
    {},
    "[e]xtract [m]ethod"
  )
end

return Key
