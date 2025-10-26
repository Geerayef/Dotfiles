vim.opt.shiftwidth = 2

local server

if vim.fn.executable("clangd") then
  F.notify("INFO", "[LSP] C|C++: Clangd.")
  server = {
    name = "Clangd LS",
    cmd = {
      "clangd",
      "--background-index",
      "--background-index-priority=low",
      "--clang-tidy",
      "--log=verbose",
    },
    on_attach = LSP.attach,
    filetypes = { "cpp", "hpp" },
    root_markers = {
      "*.cpp",
      "*.hpp",
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
    init_options = { fallback_flags = { "-std=c++20" } },
    capabilities = {
      completion = { editsNearCursor = true },
      textDocument = { switchSourceHeader = true },
    },
  }
elseif vim.fn.executable("ccls") then
  F.notify("INFO", "[LSP] C|C++: CCLS.")
  server = {
    name = "CCLS",
    cmd = { "ccls" },
    on_attach = LSP.attach,
    filetypes = { "cpp", "hpp" },
    root_markers = {
      "*.cpp",
      "*.hpp",
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
  vim.api.nvim_win_call(
    vim.api.nvim_get_current_win(),
    function() LSP.start(server) end
  )
end)
