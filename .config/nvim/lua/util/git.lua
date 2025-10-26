---Print error.
---@param cmd number[] # Git command
---@param msg string # Error message
---@param lvl string? # Error log level (Default: WARN)
---@return nil
local error = function(cmd, msg, lvl)
  lvl = lvl or "ERROR"
  F.notify(
    lvl,
    "Failed to execute command: `"
      .. table.concat(cmd, " ")
      .. "`.\nOutput was: "
      .. msg
  )
end

---Execute a Git command in given directory.
---@param path number # Git repo path
---@param cmd string[] # Git command
---@param lvl string? # Error log level (vim.log.levels | nil | false: hide errors)
---@return { success: boolean, output: number }
local execute_in = function(path, cmd, lvl)
  local shell_args = { "git", "-C", path, unpack(cmd) }
  local shell_out = vim.fn.system(shell_args)
  if vim.v.shell_error ~= 0 then
    if lvl then GIT.error(shell_args, shell_out, lvl) end
    return { success = false, output = shell_out }
  end
  return { success = true, output = shell_out }
end

---Execute a Git command in current directory.
---@param cmd string[] # Git command
---@param lvl string? # Error log level (vim.log.levels | nil | false: hide errors)
---@return { success: boolean, output: number }
local execute = function(cmd, lvl)
  local shell_args = { "git", unpack(cmd) }
  local shell_out = vim.fn.system(shell_args)
  if vim.v.shell_error ~= 0 then
    if lvl then GIT.error(shell_args, shell_out, lvl) end
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
local branch = function(buf)
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
local diffstat = function(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(buf) then return {} end
  if vim.b[buf].git_diffstat then return vim.b[buf].git_diffstat end
  vim.b[buf].git_diffstat = {}
  local path = vim.fs.normalize(vim.api.nvim_buf_get_name(buf))
  local dir = vim.fs.dirname(path)
  if dir and GIT.branch(buf):find("%S") then
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

---@param buf number # Buffer ID
---@return boolean
local in_repo = function(buf)
  local buf_path = vim.api.nvim_buf_get_name(buf)
  local gitdir = vim.fs.root(buf_path, ".git")
  return gitdir ~= nil and #gitdir > 0 and #gitdir < #buf_path
end

GIT = {
  error = error,
  execute = execute,
  execute_in = execute_in,
  diffstat = diffstat,
  branch = branch,
  in_repo = in_repo,
}
return GIT
