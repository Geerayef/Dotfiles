local keymap = vim.keymap.set

K = {}

K.LspKeymaps = function(_, bufnr)
    keymap("n", "<leader>rn", vim.lsp.buf.rename, { buffer = bufnr, desc = "[R]e[n]ame" })
    keymap("n", "<leader>ca", vim.lsp.buf.code_action, { buffer = bufnr, desc = "[C]ode [A]ction" })
    keymap("n", "<leader>gd", vim.lsp.buf.definition, { buffer = bufnr, desc = "[G]oto [D]efinition" })
    keymap("n", "<leader>gD", vim.lsp.buf.declaration, { buffer = bufnr, desc = "[G]oto [D]eclaration" })
    keymap("n", "<leader>gr", require("telescope.builtin").lsp_references, { buffer = bufnr, desc = "[G]oto [R]eferences" })
    keymap("n", "<leader>gI", vim.lsp.buf.implementation, { buffer = bufnr, desc = "[G]oto [I]mplementation" })
    keymap("n", "<leader>D", vim.lsp.buf.type_definition, { buffer = bufnr, desc = "Type [D]efinition" })
    keymap("n", "<leader>ds", require("telescope.builtin").lsp_document_symbols, { buffer = bufnr, desc = "[D]ocument [S]ymbols" })
    keymap("n", "<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, { buffer = bufnr, desc = "[W]orkspace [S]ymbols" })
    keymap("n", "<leader>waf", vim.lsp.buf.add_workspace_folder, { buffer = bufnr, desc = "[W]orkspace [A]dd [F]older" })
    keymap("n", "<leader>wrf", vim.lsp.buf.remove_workspace_folder, { buffer = bufnr, desc = "[W]orkspace [R]emove [F]older" })
    keymap("n", "<leader>wlf", function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, { buffer = bufnr, desc = "[W]orkspace [L]ist [F]olders" })
    -- See `:help K` for why this keymap
    keymap("n", "K", vim.lsp.buf.hover, { buffer = bufnr, desc = "Hover Documentation" })
    keymap("n", "<C-k>", vim.lsp.buf.signature_help, { buffer = bufnr, desc = "Signature Documentation" })
end

return K
