local clangd = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "cpp", "hpp" },
  root_patterns = { "*.clangd" },
  cmd = { "clangd", "--background-index", "--clang-tidy", "--log=verbose" },
  init_options = {
    fallback_flags = { "-std=c++17" },
  },
}

require("util.lsp").start(clangd)
