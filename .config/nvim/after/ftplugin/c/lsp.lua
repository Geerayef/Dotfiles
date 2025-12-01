vim.opt.shiftwidth = 2
---@type vim.lsp.Config
local server = {}

if vim.fn.executable("clangd") then
  F.notify("INFO", "[LSP] C|C++: Clangd.")
  server = {
    name = "Clangd LS",
    cmd = { "clangd", "--background-index", "--clang-tidy", "--log=verbose" },
    on_attach = LSP.attach,
    filetypes = { "c", "h" },
    root_markers = {
      "*.c",
      "*.h",
      "*.cmake",
      "cmake*/",
      "CMakePresets.json",
      "CTestConfig.cmake",
      "compile_commands.json",
      "compile_flags.txt",
      ".clangd",
      ".clang-tidy",
      ".clang-format",
    },
    init_options = { fallback_flags = { "-std=c11" } },
  }
elseif vim.fn.executable("ccls") then
  F.notify("INFO", "[LSP] C|C++: CCLS.")
  server = {
    name = "CCLS",
    cmd = { "ccls" },
    on_attach = LSP.attach,
    filetypes = { "c", "h" },
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
    offset_encoding = "utf-32",
    single_file_support = false,
    init_options = {
      -- compilationDatabaseDirectory = "build",
      -- index = { threads = 0 },
      clang = { excludeArgs = { "-frounding-math" } },
    },
  }
else
  server = {}
  F.notify("ERROR", "C/C++ LSP not found.")
end

vim.schedule(function()
  vim.api.nvim_win_call(vim.api.nvim_get_current_win(), function() LSP.start(server) end)
end)
