local M = {}

---Print error.
---@param cmd number[] # Git command
---@param msg string # Error message
---@param lvl string? # Error log level (Default: WARN)
---@return nil
function M.error(cmd, msg, lvl)
  lvl = lvl or "WARN"
  F.Notify(
    lvl,
    "Failed to execute command: " .. table.concat(cmd, " ") .. ".\n" .. msg
  )
end

---Execute a Git command in given directory.
---@param path number # Git repo path
---@param cmd string[] # Git command
---@param lvl string? # Error log level (vim.log.levels | nil | false: hide errors)
---@return { success: boolean, output: number }
function M.execute_in(path, cmd, lvl)
  local shell_args = { "git", "-C", path, unpack(cmd) }
  local shell_out = vim.fn.system(shell_args)
  if vim.v.shell_error ~= 0 then
    if lvl then M.error(shell_args, shell_out, lvl) end
    return { success = false, output = shell_out }
  end
  return { success = true, output = shell_out }
end

---Execute a Git command in current directory.
---@param cmd string[] # Git command
---@param lvl string? # Error log level (vim.log.levels | nil | false: hide errors)
---@return { success: boolean, output: number }
function M.execute(cmd, lvl)
  local shell_args = { "git", unpack(cmd) }
  local shell_out = vim.fn.system(shell_args)
  if vim.v.shell_error ~= 0 then
    if lvl then M.error(shell_args, shell_out, lvl) end
    return { success = false, output = shell_out }
  end
  return { success = true, output = shell_out }
end

vim.api.nvim_create_autocmd("FileChangedShellPost", {
  group = vim.api.nvim_create_augroup("RefreshGitBranchCache", {}),
  callback = function(info) vim.b[info.buf].git_branch = nil end,
})

---[async] Get current branch name.
---@param buf integer? # Buffer handle (Default: current buffer)
---@return string # Git branch name
function M.branch(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(buf) then return "" end
  if vim.b[buf].git_branch then return vim.b[buf].git_branch end
  vim.b[buf].git_branch = ""
  local dir = vim.fs.dirname(vim.api.nvim_buf_get_name(buf))
  if dir then
    pcall(
      vim.system,
      { "git", "-C", dir, "rev-parse", "--abbrev-ref", "HEAD" },
      { stderr = false },
      function(err)
        local branch = err.stdout:gsub("\n.*", "")
        pcall(vim.api.nvim_buf_set_var, buf, "git_branch", branch)
      end
    )
  end
  return vim.b[buf].git_branch
end

vim.api.nvim_create_autocmd({ "BufWrite", "FileChangedShellPost" }, {
  group = vim.api.nvim_create_augroup("RefreshGitDiffCache", {}),
  callback = function(info) vim.b[info.buf].git_diffstat = nil end,
})

---[async] Get diff stats for current buffer.
---@param buf integer? # Buffer handle (Default: current buffer)
---@return {added: integer?, removed: integer?, changed: integer?} # Diff stats
function M.diffstat(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(buf) then return {} end
  if vim.b[buf].git_diffstat then return vim.b[buf].git_diffstat end
  vim.b[buf].git_diffstat = {}
  local path = vim.fs.normalize(vim.api.nvim_buf_get_name(buf))
  local dir = vim.fs.dirname(path)
  if dir and M.branch(buf):find("%S") then
    pcall(vim.system, {
      "git",
      "-C",
      dir,
      "--no-pager",
      "diff",
      "-U0",
      "--no-color",
      "--no-ext-diff",
      "--",
      path,
    }, { stderr = false }, function(out)
      local stat = { added = 0, removed = 0, changed = 0 }
      for _, line in ipairs(vim.split(out.stdout, "\n")) do
        if line:find("^@@ ") then
          local num_lines_old, num_lines_new =
            line:match("^@@ %-%d+,?(%d*) %+%d+,?(%d*)")
          num_lines_old = tonumber(num_lines_old) or 1
          num_lines_new = tonumber(num_lines_new) or 1
          local num_lines_changed = math.min(num_lines_old, num_lines_new)
          stat.changed = stat.changed + num_lines_changed
          if num_lines_old > num_lines_new then
            stat.removed = stat.removed + num_lines_old - num_lines_changed
          else
            stat.added = stat.added + num_lines_new - num_lines_changed
          end
        end
      end
      pcall(vim.api.nvm_buf_set_var, buf, "git_diffstat", stat)
    end)
  end
  return vim.b[buf].git_diffstat
end

return M
