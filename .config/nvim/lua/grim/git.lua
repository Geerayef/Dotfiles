---Execute a Git command in current directory.
---@param cmd string[] # Git command
---@return { success: boolean, output: string }
local execute = function(cmd)
  local shell_args = { "git", unpack(cmd) }
  local shell_out = vim.fn.system(shell_args)
  if vim.v.shell_error ~= 0 then return { success = false, output = shell_out } end
  return { success = true, output = shell_out }
end

---Execute a Git command in given directory.
---@param path string # Git repo path
---@param cmd string[] # Git command
---@return { success: boolean, output: string }
local execute_in = function(path, cmd)
  local shell_args = { "git", "-C", path, unpack(cmd) }
  local shell_out = vim.fn.system(shell_args)
  if vim.v.shell_error ~= 0 then return { success = false, output = shell_out } end
  return { success = true, output = shell_out }
end

---Get current branch name.
---@param bufid integer? # Buffer handle (Default: current buffer)
---@return string # Git branch name
local branch = function(bufid)
  bufid = bufid or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(bufid) then return "" end
  if vim.b[bufid].git_branch then return vim.b[bufid].git_branch end
  vim.b[bufid].git_branch = ""
  local dir = vim.fs.dirname(vim.api.nvim_buf_get_name(bufid))
  local name = vim
    .system(
      { "git", "-C", dir, "rev-parse", "--abbrev-ref", "HEAD" },
      { stderr = false },
      function(out) return out.stdout and string.gsub(out.stdout, "\n.*", "") end
    )
    :wait()
  return name
end

---Get diff stats for current buffer.
---@param bufid integer? # Buffer handle (Default: current buffer)
---@return {added: integer, removed: integer, changed: integer} # Diff stats
local diffstat = function(bufid)
  bufid = bufid or vim.api.nvim_get_current_buf()
  if not vim.api.nvim_buf_is_valid(bufid) then return { added = 0, removed = 0, changed = 0 } end
  if vim.b[bufid].git_diffstat then return vim.b[bufid].git_diffstat end
  vim.b[bufid].git_diffstat = {}
  local path = vim.fs.normalize(vim.api.nvim_buf_get_name(bufid))
  local dir = vim.fs.dirname(path)
  local stat = { added = 0, removed = 0, changed = 0 }
  if dir and branch(bufid):find("%S") then
    stat = vim
      .system(
        { "git", "-C", dir, "--no-pager", "diff", "-U0", "--no-color", "--no-ext-diff", "--", path },
        { stderr = false },
        function(out)
          for _, line in ipairs(vim.split(out.stdout, "\n")) do
            if line:find("^@@ ") then
              local num_lines_old, num_lines_new = line:match("^@@ %-%d+,?(%d*) %+%d+,?(%d*)")
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
          return stat
        end
      )
      :wait()
  end
  return stat
end

---@param bufid integer # Buffer ID
---@return boolean
local versioned_p = function(bufid)
  local buf_path = vim.api.nvim_buf_get_name(bufid)
  if buf_path == "" then return false end
  local buf_dir = vim.fn.fnamemodify(buf_path, ":h")
  local result = vim
    .system({ "git", "-C", buf_dir, "rev-parse", "--is-inside-work-tree" }, { text = true })
    :wait()
  return result.code == 0 and string.match(result.stdout, "true") ~= nil
end

---GRIM.git provides utilities for working with Git.
---@class GRIM.git
---@field execute fun(cmd: string[]): { output: string, success: boolean }
---@field execute_in fun(path: string, cmd: string[]): { output: string, success: boolean }
---@field diffstat fun(buf: integer?): { added: integer, changed: integer, removed: integer }
---@field branch fun(buf: integer?): string
---@field versioned_p fun(buf: integer): boolean
return {
  execute = execute,
  execute_in = execute_in,
  diffstat = diffstat,
  branch = branch,
  versioned_p = versioned_p,
}
