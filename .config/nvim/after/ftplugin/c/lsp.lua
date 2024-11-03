local server

if vim.fn.executable("clangd") then
  require("core.func").Notify("INFO", "Using clangd for C/C++.")
  server = {
    on_attach = require("core.func").LSPAttach,
    filetypes = { "c", "h" },
    root_patterns = {
      "compile_commands.json",
      ".clangd",
      ".clang-tidy",
      ".clang-format",
      "compile_flags.txt",
    },
    cmd = { "clangd", "--background-index", "--clang-tidy", "--log=verbose" },
    init_options = { fallback_flags = { "-std=c11" } },
  }
elseif vim.fn.executable("ccls") then
  require("core.func").Notify("INFO", "Using CCLS for C/C++.")
  server = {
    on_attach = require("core.func").LSPAttach,
    filetypes = { "c", "h" },
    root_patterns = { ".ccls", "compile_commands.json", "compile_flags.txt" },
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

require("util.lsp").start(server)
