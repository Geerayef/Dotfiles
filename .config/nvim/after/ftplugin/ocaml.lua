-- ~  OCaml Merlin & OCP indent

vim.g.opamshare = vim.fn.substitute(vim.fn.system("opam var share"), "\n$", "", "")
local merlin_path = vim.g.opamshare .. "/merlin/vim"
vim.opt.rtp:append(merlin_path)
local merlin_doc = merlin_path .. "/doc"
vim.cmd({ cmd = "helptags", args = { merlin_doc } })
local ocp_indent_path = vim.fn.substitute(vim.fn.system("opam var ocp-indent:share"), "\n$", "", "")
vim.opt.rtp:append(ocp_indent_path)

-- local ocamllsp = vim.api.nvim_create_augroup("OCamlLSP")
-- vim.api.nvim_create_autocmd("BufReadPost", {
--   group = ocamllsp,
--   callback = function()
--     return vim.lsp.start({
--       name = "ocamllsp",
--       cmd = "ocamllsp",
--       autostart = true,
--       on_attach = F.LspAttach,
--       filetypes = { "ocaml", "ocaml.menhir", "ocaml.interface", "ocaml.ocamllex", "reason", "dune" },
--       root_dir = vim.fs.dirname(vim.fs.find({
--         "*.ml",
--         "*.mli",
--         "*.opam",
--         "dune",
--         ".ocamlformat",
--         "ocamlformat",
--         "esy.json",
--         "package.json",
--         "dune-project",
--         "dune-workspace",
--         ".git",
--       }, { upward = true })[1]),
--     })
--   end,
-- })
