---Execute a Git command in current directory.
---@generic T
---@param on fun(out: vim.SystemCompleted): T? # Optional function for processing output.
---@param cmd string[] # Git command.
---@param opts vim.SystemOpts? # Optional options for command execution.
---@return T
local execute = function(on, cmd, opts)
  local default = { text = true, stderr = false }
  on = on ~= nil and on or function(o) return o end --[[@as fun(out: vim.SystemCompleted): T]]
  return on(
    vim
      .system(
        { "git", unpack(cmd) },
        opts ~= nil and vim.tbl_deep_extend("force", default, opts) or default
      )
      :wait()
  )
end

---Get current branch name.
---@param bufid integer # Buffer handle (Default: current buffer)
---@return string # Git branch name
local branch = function(bufid)
  if not (bufid and vim.api.nvim_buf_is_valid(bufid)) then
    bufid = vim.api.nvim_get_current_buf()
  end
  return execute(
    function(o)
      return (function(x, _) return x end)(string.gsub(o.stdout, "\n.*", ""))
    end,
    { "-C", vim.fs.dirname(vim.api.nvim_buf_get_name(bufid)), "rev-parse", "--abbrev-ref", "HEAD" }
  )
end

---Get diff stats for current buffer.
---@param bufid integer # Buffer handle (Default: current buffer)
---@return {added: integer, removed: integer, changed: integer} # Diff stats
local diffstat = function(bufid)
  if not (bufid and vim.api.nvim_buf_is_valid(bufid)) then
    bufid = vim.api.nvim_get_current_buf()
  end
  local path = vim.fs.normalize(vim.api.nvim_buf_get_name(bufid))
  local dir = vim.fs.dirname(path)
  local stat = { added = 0, removed = 0, changed = 0 }
  if dir and branch(bufid):find("%S") then
    stat = execute(
      ---@return { added: integer, removed: integer, changed: integer }
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
      end,
      { "-C", dir, "--no-pager", "diff", "-U0", "--no-color", "--no-ext-diff", "--", path }
    )
  end
  return stat
end

---Report whether a buffer bufid is versioned in Git.
---@param bufid integer # Buffer ID
---@return boolean
local versioned_p = function(bufid)
  if not (bufid and vim.api.nvim_buf_is_valid(bufid)) then
    bufid = vim.api.nvim_get_current_buf()
  end
  local buf_path = vim.api.nvim_buf_get_name(bufid)
  if buf_path == "" then return false end
  return execute(
    function(o) return o.code == 0 and string.match(o.stdout, "true") ~= nil end,
    { "-C", vim.fs.dirname(buf_path), "rev-parse", "--is-inside-work-tree" }
  )
end

---@package GRIM.git
---GRIM.git provides utilities for working with Git.
---@class GRIM.git
---@generic T
---@field execute fun(cmd: string[], opts?: vim.SystemOpts, on?: fun(out: vim.SystemCompleted): T): T
---@field branch fun(buf: integer?): string
---@field diffstat fun(buf: integer?): { added: integer, changed: integer, removed: integer }
---@field versioned_p fun(buf: integer): boolean
return {
  execute = execute,
  branch = branch,
  diffstat = diffstat,
  versioned_p = versioned_p,
}
