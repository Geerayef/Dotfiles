local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

--- ~ Tree-Sitter auto-start
autocmd("FileType", {
  desc = "Automatically start Tree-Sitter",
  pattern = GRIM.static.treesitter_grammars,
  group = augroup("AutoTreeSitter", { clear = true }),
  callback = function() vim.treesitter.start() end,
})

--- ~ Large-file performance
autocmd("BufReadPre", {
  desc = "Better handle large files.",
  group = augroup("LargeFileSettings", { clear = true }),
  pattern = "*",
  callback = function(info)
    vim.b.bigfile = false
    if GRIM.fs.file_large_p(info.buf) then
      vim.b.bigfile = true
      vim.opt_local.spell = false
      vim.opt_local.swapfile = false
      vim.opt_local.undofile = false
      vim.opt_local.breakindent = false
      vim.opt_local.colorcolumn = ""
      vim.opt_local.statuscolumn = ""
      vim.opt_local.signcolumn = "no"
      vim.opt_local.foldcolumn = "0"
      vim.opt_local.winbar = ""
      vim.opt_local.syntax = ""
      autocmd("BufReadPost", {
        once = true,
        buffer = info.buf,
        callback = function()
          vim.opt_local.syntax = ""
          return true
        end,
      })
    end
  end,
})

--- ~ Highlight on yank
autocmd("TextYankPost", {
  desc = "Highlight yanked text.",
  group = augroup("YankHighlight", { clear = true }),
  pattern = "*",
  callback = function() vim.hl.on_yank() end,
})

--- ~ Auto cd
autocmd({ "TabEnter", "BufEnter", "BufWinEnter" }, {
  desc = "Automatically change current working directory based on predefined markers.",
  group = augroup("AutoCWD", { clear = true }),
  pattern = "*",
  callback = function(info)
    if info.file == "" or vim.bo[info.buf].bt ~= "" then return end
    local win = vim.api.nvim_get_current_win()
    vim.schedule(function()
      if
        not vim.api.nvim_buf_is_valid(info.buf)
        or not vim.api.nvim_win_is_valid(win)
        or not vim.api.nvim_win_get_buf(win) == info.buf
      then
        return
      end
      vim.api.nvim_win_call(win, function()
        local dir_from = vim.fn.getcwd(0)
        local dir_to = GRIM.fs.root(info.file)
        if dir_to ~= nil and dir_to ~= "" and dir_to ~= dir_from then pcall(vim.cmd.cd, dir_to) end
      end)
    end)
  end,
})

--- ~ Close special buffers with `q`
autocmd("FileType", {
  desc = "Close specific buffers with `q`.",
  group = augroup("qCloseSpecialFT", { clear = true }),
  pattern = table.concat({
    "qf",
    "git",
    "lazy",
    "help",
    "fugitive",
    "checkhealth",
    "NeogitStatus",
  }, ","),
  callback = function()
    vim.keymap.set("n", "q", "<C-w>c", {
      buffer = 0,
      desc = "Close current buffer.",
    })
  end,
})

--- ~ Apply custom UI highlights
autocmd("ColorScheme", {
  desc = "Apply custom highlights after loading the main colorscheme.",
  group = augroup("CustomHighlights", { clear = true }),
  pattern = "*",
  callback = function()
    local road = require("clrs.road")
    local rb, rp = road.base, road.palette
    local hl = vim.api.nvim_set_hl
    -- UI
    hl(0, "Normal", { fg = rb.mintCream, bg = rb.dragonInk })
    hl(0, "NormalNC", { link = "Normal" })
    hl(0, "NormalFloat", { link = "Normal" })
    hl(0, "FloatBorder", { link = "Normal" })
    hl(0, "CursorLine", { bg = rp.jet[200] })
    hl(0, "CursorLineNr", { fg = rb.lotusYellow, bold = true })
    hl(0, "TabLineSel", { fg = rb.lotusYellow })
    hl(0, "SignColumn", { link = "Normal" })
    hl(0, "WinSeparator", { link = "Normal" })
    hl(0, "MatchParen", {
      fg = rb.mintCream,
      bg = rp.charcoal[200],
      bold = true,
      underline = true,
    })
    hl(0, "RenderMarkdownCode", { bg = rb.dragonInk })
    hl(0, "GitSignsCurrentLineBlame", { link = "Comment" })
    hl(0, "LspSignatureActiveParameter", {
      fg = rp.lotusYellow[400],
      bg = rb.dragonInk,
      bold = true,
    })
    hl(0, "ActionPreviewTitle", {
      fg = rp.lotusYellow[400],
      bg = rb.dragonInk,
      bold = true,
    })
    -- Syntax
    vim.cmd.highlight({ args = { "Keyword", "cterm=bold", "gui=bold" } })
    hl(0, "@keyword", { bold = true })
    hl(0, "Comment", { fg = rb.paynesGray, italic = true })
    hl(0, "Function", { fg = rp.lotusYellow[400] })
    hl(0, "@function", { fg = rp.lotusYellow[400] })
    hl(0, "@function.call", { fg = rp.lotusYellow[400] })
    hl(0, "@function.macro", { fg = rp.lotusYellow[400] })
    hl(0, "@function.builtin", { fg = rp.lotusYellow[400] })
  end,
})

-- From GRIM.GIT
-- autocmd("FileChangedShellPost", {
--   group = augroup("RefreshGitBranchCache", { clear = true }),
--   callback = function(info) vim.b[info.buf].git_branch = nil end,
-- })
--
-- autocmd({ "BufWrite", "FileChangedShellPost" }, {
--   group = augroup("RefreshGitDiffCache", { clear = true }),
--   callback = function(info) vim.b[info.buf].git_diffstat = nil end,
-- })
