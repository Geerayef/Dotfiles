local lsp = require("util.lsp")
local lua_ls = {
  on_attach = F.LspAttach,
  filetypes = { "lua" },
  root_patterns = { "*.lua" },
  settings = {
    Lua = {
      completion = {
        enable = true,
        callSnippet = "Both",
        keywordSnippet = "Both",
        displayContext = 2,
      },
      diagnostics = {
        enable = true,
        globals = { "vim", "jit" },
        neededFileStatus = "Opened",
      },
      runtime = {
        version = "LuaJIT",
        path = vim.split(package.path, ";"),
      },
      workspace = {
        library = {
          vim.env.VIMRUNTIME,
          vim.fn.expand("$VIMRUNTIME/lua"),
          vim.fn.expand("$VIMRUNTIME/lua/vim/lsp"),
          vim.fn.expand("$XDG_CONFIG_HOME") .. "/nvim/lua",
          "${3rd}/luv/library",
        },
        ignoreSubmodules = false,
        preloadFileSize = 1000,
        checkThirdParty = false,
      },
      telemetry = { enable = false },
      hint = { enable = true, setType = true },
    },
  },
  cmd = { "lua-language-server" },
}

lsp.start(lua_ls)
