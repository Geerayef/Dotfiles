local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- ~  Highlight on yank

local hl_group = augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", {
  callback = function() vim.highlight.on_yank() end,
  group = hl_group,
  pattern = "*",
})

-- ~  Improve performance in large files

local group = vim.api.nvim_create_augroup("LargeFileAutocmds", {})
local old_eventignore = vim.o.eventignore
local largefile_opened = false

vim.api.nvim_create_autocmd({ "BufReadPre" }, {
  group = group,
  callback = function(ev)
    if ev.file then
      local status, size = pcall(function() return vim.loop.fs_stat(ev.file).size end)
      if status and size > 1024 * 1024 then
        vim.wo.wrap = false
        old_eventignore = vim.o.eventignore
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

vim.api.nvim_create_autocmd({ "BufWinEnter" }, {
  group = group,
  callback = function()
    if largefile_opened then
      largefile_opened = false
      vim.o.eventignore = nil
    end
  end,
})

vim.api.nvim_create_autocmd({ "BufEnter" }, {
  group = group,
  callback = function(ev)
    local byte_size = vim.api.nvim_buf_get_offset(ev.buf, vim.api.nvim_buf_line_count(ev.buf))
    if byte_size > 1024 * 1024 then
      if vim.g.loaded_matchparen then vim.cmd("NoMatchParen") end
    else
      if not vim.g.loaded_matchparen then vim.cmd("DoMatchParen") end
    end
  end,
})
