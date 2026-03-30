-- Options

vim.opt.shiftwidth = 2

-- LSP

if vim.fn.executable("clangd") == 1 then
  F.notify("INFO", "[LSP] C|C++: Clangd.")
  vim.lsp.enable("clangd", true)
elseif vim.fn.executable("ccls") == 1 then
  F.notify("INFO", "[LSP] C|C++: CCLS.")
  vim.lsp.enable("ccls", true)
else
  F.notify("ERROR", "C/C++ LSP not found.")
end
