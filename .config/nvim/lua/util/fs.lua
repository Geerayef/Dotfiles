---Compute the path of file's root directory.
---@param file string # File path
---@param root_markers string[]? # Files or directories marking the root
---@return string? # Absolute path of the root directory
local root = function(file, root_markers)
  if not file or file == "" or not vim.uv.fs_stat(file) then return nil end
  root_markers = root_markers or S.root_markers
  if vim.tbl_contains(root_markers, vim.fs.basename(file)) then
    return vim.fs.dirname(file)
  end
  local proximity_threshold = 2
  local closest = ""
  local closest_proximity = 32
  local root = ""
  local root_depth = 0
  local root_proximity = 0
  local _, file_depth = file:gsub("/", "")
  local mark_path = ""
  for _, mark in ipairs(root_markers) do
    mark_path = vim.fs.find(mark, {
      path = file,
      upward = true,
      type = mark:match("/$") and "directory" or "file",
    })[1]
    if mark_path ~= nil and mark_path ~= "" then
      root = vim.fs.dirname(mark_path)
      if root ~= nil and root ~= "" then
        root = vim.uv.fs_realpath(root) --[[@as string]]
        _, root_depth = root:gsub("/", "")
        root_proximity = file_depth - root_depth
        if root_proximity <= closest_proximity then
          if root_proximity <= proximity_threshold then return root end
          closest_proximity = root_proximity
          closest = root
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
  local file = io.open(path, "r")
  if not file then return nil end
  local content = file:read("*a")
  file:close()
  return content or ""
end

---Write string into file.
---TODO: Check if the given path already exists first!
---@param path string # File path relative to CWD
---@param content string
---@return boolean success
local file_write = function(path, content)
  local file = io.open(path, "w")
  if not file then return false end
  file:write(content)
  file:close()
  return true
end

---@param buf number # Buffer ID
---@return boolean
local file_large = function(buf)
  local size_threshold = 1024 * 1024
  local ok, stat = pcall(vim.uv.fs_stat, vim.api.nvim_buf_get_name(buf))
  return ok and stat ~= nil and stat.size > size_threshold
end

FS = {
  root = root,
  file_read = file_read,
  file_write = file_write,
  file_large = file_large,
}
return FS
