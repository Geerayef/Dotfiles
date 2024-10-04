local clangd = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "cpp", "hpp" },
  root_patterns = {
    "compile_commands.json",
    ".clangd",
    ".clang-tidy",
    ".clang-format",
    "compile_flags.txt",
  },
  cmd = {
    "clangd",
    "--background-index",
    "--background-index-priority=low",
    "--clang-tidy",
    "--log=verbose",
  },
  init_options = { fallback_flags = { "-std=c++20" } },
  capabilities = {
    completion = { editsNearCursor = true },
    textDocument = { switchSourceHeader = true },
  },
}

require("util.lsp").start(clangd)
