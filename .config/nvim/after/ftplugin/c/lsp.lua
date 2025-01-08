vim.opt.shiftwidth = 2

local server

if vim.fn.executable("clangd") then
  require("core.func").Notify("INFO", "[LSP] C|C++: Clangd.")
  server = {
    on_attach = require("core.func").LSPAttach,
    filetypes = { "c", "h" },
    root_patterns = {
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
    cmd = { "clangd", "--background-index", "--clang-tidy", "--log=verbose" },
    init_options = { fallback_flags = { "-std=c11" } },
  }
elseif vim.fn.executable("ccls") then
  require("core.func").Notify("INFO", "[LSP] C|C++: CCLS.")
  server = {
    on_attach = require("core.func").LSPAttach,
    filetypes = { "c", "h" },
    root_patterns = {
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
    cmd = { "ccls" },
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
  F.Notify("ERROR", "C/C++ LSP not found.")
end

vim.schedule(function()
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() require("util.lsp").start(server) end
  )
end)
