local setup, nvimtree = pcall(require, "nvim-tree")
if not setup then
    return
end

nvimtree.setup({
    sort_by = "case_sensitive",
    view = {
        width = 35,
        mappings = {
            list = {
                -- My mappings
            }
        }
    },
    renderer = {
        group_empty = true,
        highlight_git = true,
        highlight_opened_files = "name",
        highlight_modified = "icon",
    },
    diagnostics = {
        enable = true,
    },
    filters = {
    },
    modified = {
        enable = true,
        show_on_open_dirs = false,
    },
})
