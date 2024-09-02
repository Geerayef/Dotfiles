local clangd = {
  on_attach = require("core.func").LspAttach,
  filetypes = { "c", "h" },
  root_patterns = { "*.clangd" },
  cmd = { "clangd", "--background-index", "--clang-tidy", "--log=verbose" },
  init_options = {
    fallback_flags = { "-std=c11" },
  },
}

require("util.lsp").start(clangd)
