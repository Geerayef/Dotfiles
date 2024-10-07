local server

if vim.fn.executable("ccls") then
  server = {
    on_attach = require("core.func").LspAttach,
    filetypes = { "c", "h" },
    root_patterns = { ".ccls", "compile_commands.json", "compile_flags.txt" },
    cmd = { "ccls" },
    offset_encoding = "utf-32",
    single_file_support = false,
    init_options = {
      compilationDatabaseDirectory = "build",
      index = { threads = 0 },
      clang = { excludeArgs = { "-frounding-math" } },
    },
  }
elseif vim.fn.executable("clangd") then
  server = {
    on_attach = require("core.func").LspAttach,
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
else
  server = {}
  F.Notify("ERROR", "LSP for C/CPP not found.")
end

require("util.lsp").start(server)
