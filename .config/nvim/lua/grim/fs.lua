---Compute the path of file's root directory.
---@param path string # File path
---@param root_markers string[]? # Files or directories marking the root
---@return string? # Absolute path of the root directory
local root = function(path, root_markers)
  if not path or path == "" or not vim.uv.fs_stat(path) then return nil end
  if not root_markers or #root_markers == 0 then
    root_markers = vim.fn.flatten(GRIM.static.root_markers) ---@as string[]
  end
  if vim.tbl_contains(root_markers, vim.fs.basename(path)) then return vim.fs.dirname(path) end
  local proximity_threshold = 2
  local closest = ""
  local proximity_closest = 16
  local path_root = ""
  local root_depth = 0
  local proximity_root = 0
  local _, file_depth = path:gsub("/", "")
  local mark_path = ""
  for _, mark in ipairs(root_markers) do
    mark_path = vim.fs.find(mark, {
      path = path,
      upward = true,
      type = mark:match("/$") and "directory" or "file",
    })[1] ---@as string
    if mark_path ~= nil and mark_path ~= "" then
      path_root = vim.fs.dirname(mark_path)
      if path_root ~= nil and path_root ~= "" then
        path_root = vim.uv.fs_realpath(path_root) --[[@as string]]
        _, root_depth = path_root:gsub("/", "")
        proximity_root = file_depth - root_depth
        if proximity_root <= proximity_closest then
          if proximity_root <= proximity_threshold then return path_root end
          proximity_closest = proximity_root
          closest = path_root
        end
      end
    end
  end
  return closest
end

---Read file contents.
---@param path string # File path relative to CWD
---@return string?
local file_read = function(path)
  local file, err = io.open(path, "r")
  if err ~= nil then return err end
  if not file then return nil end
  local content = file:read("*a")
  file:close()
  return content
end

---Write string into file.
---@param path string # File path relative to CWD
---@param content string
---@return boolean success
local file_write = function(path, content)
  local file, err = io.open(path, "w")
  if err ~= nil then return false end
  if not file then return false end
  file:write(content)
  file:close()
  return true
end

---@param buf integer # Buffer ID
---@return boolean
local file_large_p = function(buf)
  local size_threshold = 1024 * 1024
  local ok, stat = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
  return ok and stat ~= nil and stat.size > size_threshold
end

---GRIM.fs provides file system interoperation utilities.
---@class GRIM.fs
---@field root fun(path: string, root_markers: string[]?): string
---@field file_read fun(path: string): string?
---@field file_write fun(path: string, content: string): boolean
---@field file_large_p fun(buf: integer): boolean
return {
  root = root,
  file_read = file_read,
  file_write = file_write,
  file_large_p = file_large_p,
}
