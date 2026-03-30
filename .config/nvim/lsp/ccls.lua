---@type vim.lsp.Config
return {
  name = "CCLS",
  cmd = { "ccls" },
  capabilities = { offsetEncoding = { "utf-8", "utf-16", "utf-32" } },
  single_file_support = false,
  workspace_required = true,
  init_options = {
    compilationDatabaseDirectory = "build",
    index = { threads = 0 },
    clang = { excludeArgs = { "-frounding-math" } },
    fallback_flags = {
      (function(buf)
        local ft = vim.bo[buf].ft
        if ft == "c" then
          return "-stc=c18"
        elseif ft == "cpp" then
          return "-stc=c++20"
        end
      end)(vim.api.nvim_get_current_buf()),
    },
  },
  filetypes = { "c", "cpp" },
  root_markers = {
    "*.c",
    "*.h",
    ".ccls",
    "*.cmake",
    "cmake*/",
    "CMakePresets.json",
    "CTestConfig.cmake",
    "compile_commands.json",
    "compile_flags.txt",
  },
}
