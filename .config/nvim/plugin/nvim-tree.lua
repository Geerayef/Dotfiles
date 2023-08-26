local setup, nvimtree = pcall(require, "nvim-tree")
if not setup then
    return
end

nvimtree.setup({
    disable_netrw = true,
    hijack_netrw = false,
    sort_by = "case_sensitive",
    view = {
        width = 35,
        side = "right",
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
    modified = {
        enable = true,
        show_on_open_dirs = false,
    },
    actions = {
        open_file = {
            quit_on_open = true,
        }
    }
})
