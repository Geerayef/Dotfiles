local M = {}

M.root_patterns = {
  -- Common directories
  ".git/",
  -- "src/",
  -- "bin/",
  -- "lib/",
  -- "build/",
  -- "out/",
  -- "test/",
  -- "scripts/",
  -- "assets/",
  -- Lua
  "lua/",
  "init.lua",
  "stylua.toml",
  "lazy-lock.json",
  -- OCaml
  "dune",
  ".ocamlformat",
  ".ocp-indent",
  "dune-project",
  "mantle.opam",
  -- Make
  "Makefile",
  "makefile",
  "MAKEFILE",
  "README.md",
  "README.org",
  "README.txt",
  "README.pdf",
  ".editorconfig",
  ".gitignore",
}

-- TODO: Use quick sort.
-- ---Sort candidates by proximity to the path.
-- ---@param path string? # File/Directory path
-- ---@param candidates string[]? # List of paths
-- ---@return string[]? nil # Sorted list of paths | nil if invalid path
-- local function sort_proximity(path, candidates)
--   if not path or path == "" then return nil end
--   if not candidates or #candidates == 0 then return nil end
--   for i, root in ipairs(candidates) do
--     local dir_path = vim.uv.fs_realpath(root)
--   end
-- end

---Compute root directory.
---@param path string?
---@param patterns string[]? # Root patterns
---@return string? nil # If not found
function M.proj_dir(path, patterns)
  if not path or path == "" then return nil end
  patterns = patterns or M.root_patterns
  local stat = vim.uv.fs_stat(path)
  if not stat then return end
  local dir_path = stat.type == "directory" and path or vim.fs.dirname(path)
  -- TODO: Select closest root instead of first-matched root.
  for _, pattern in ipairs(patterns) do
    local root = vim.fs.find(pattern, {
      path = dir_path,
      upward = true,
      type = pattern:match("/$") and "directory" or "file",
    })[1]
    if root and vim.uv.fs_stat(root) then
      local dir_parent = vim.fs.dirname(root)
      return dir_parent and vim.uv.fs_realpath(dir_parent) --[[@as string]]
    end
  end
end

---Read file contents.
---@param path string
---@return string?
function M.read_file(path)
  local file = io.open(path, "r")
  if not file then return nil end
  local content = file:read("*a")
  file:close()
  return content or ""
end

---Write string into file.
---@param path string
---@return boolean success
function M.write_file(path, str)
  local file = io.open(path, "w")
  if not file then return false end
  file:write(str)
  file:close()
  return true
end

return M
