local status, gitsigns = pcall(require, "gitsigns")
if not status then
    return
end

gitsigns.setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "X" },
    topdelete = { text = "^" },
    changedelete = { text = "¬" },
    untracked = { text = "?" }
  },
}
