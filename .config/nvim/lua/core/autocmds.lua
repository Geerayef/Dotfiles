local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- ~  Improve performance in large files

F.mk_autocmd("LargeFileSettings", {
  "BufReadPre",
  {
    desc = "Settings for handling large files.",
    callback = function(info)
      vim.b.bigfile = false
      -- local stat = vim.uv.fs_stat(info.match)
      -- if stat and stat.size > 1024000 then
      if F.IsBigBuff(info.buf) then
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
  },
})

-- ~  Highlight on yank

local yankhl = augroup("YankHighlight", { clear = true })
autocmd(
  "TextYankPost",
  {
    callback = function() vim.highlight.on_yank() end,
    group = yankhl,
    pattern = "*",
  }
)

-- ~  Auto cd

F.mk_autocmd("AutoCWD", {
  { "BufWinEnter", "FileChangedShellPost" },
  {
    pattern = "*",
    desc = "Automatically change local current directory.",
    callback = function(info)
      if info.file == "" or vim.bo[info.buf].bt ~= "" then return end
      local buf = info.buf
      local win = vim.api.nvim_get_current_win()
      local fs = require("util.fs")
      vim.schedule(function()
        if
          not vim.api.nvim_buf_is_valid(buf)
          or not vim.api.nvim_win_is_valid(win)
          or not vim.api.nvim_win_get_buf(win) == buf
        then
          return
        end
        vim.api.nvim_win_call(win, function()
          local current_dir = vim.fn.getcwd(0)
          local target_dir = fs.proj_dir(info.file) or vim.fs.dirname(info.file)
          local stat = target_dir and vim.uv.fs_stat(target_dir)
          if stat and stat.type == "directory" and current_dir ~= target_dir then
            pcall(vim.cmd.lcd, target_dir)
          end
        end)
      end)
    end,
  },
})
