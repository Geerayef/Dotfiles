---@type vim.lsp.Config
return {
  name = "Clangd LS",
  cmd = { "clangd", "--background-index", "--clang-tidy", "--log=verbose" },
  init_options = {
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
  on_init = function(client, init_result)
    if init_result.offsetEncoding then client.offset_encoding = init_result.offsetEncoding end
  end,
  capabilities = {
    offsetEncoding = { "utf-8", "utf-16" },
    textDocument = { completion = { editsNearCursor = true }, switchSourceHeader = true },
  },
  filetypes = { "c", "cpp" },
  root_markers = {
    "*.c",
    "*.h",
    "*.cpp",
    "*.hpp",
    "*.cmake",
    "cmake*/",
    "CMakePresets.json",
    "CTestConfig.cmake",
  },
}
