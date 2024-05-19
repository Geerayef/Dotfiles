local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- ~  Highlight on yank

local yankhl = augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", { callback = function() vim.highlight.on_yank() end, group = yankhl, pattern = "*" })

-- ~  Improve performance in large files

local group = vim.api.nvim_create_augroup("LargeFileAutocmds", {})
local largefile_opened = false

vim.api.nvim_create_autocmd({ "BufReadPre" }, {
  group = group,
  callback = function(ev)
    if ev.file then
      local status, size = pcall(function() return vim.uv.fs_stat(ev.file).size end)
      if status and size > 1024 * 1024 then
        vim.wo.wrap = false
        largefile_opened = true
        vim.o.eventignore = "FileType"
        vim.bo.swapfile = false
        vim.bo.bufhidden = "unload"
        vim.bo.buftype = "nowrite"
        vim.bo.undolevels = -1
      end
    end
  end,
})

vim.api.nvim_create_autocmd("BufWinEnter", {
  group = group,
  callback = function()
    if largefile_opened then
      largefile_opened = false
      vim.o.eventignore = nil
    end
  end,
})
