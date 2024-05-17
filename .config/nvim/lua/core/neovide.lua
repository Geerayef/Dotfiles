-- vim.o.guifont = "IosevkaTerm Nerd Font Mono:24"
vim.g.neovide_refresh_rate = 60
vim.g.neovide_refresh_rate_idle = 5
vim.g.neovide_fullscreen = false
vim.g.neovide_scale_factor = 1.2
vim.g.neovide_transparency = 1

vim.g.neovide_theme = "dark"
vim.g.neovide_text_gamma = 0
vim.g.neovide_text_contrast = 0.5

vim.g.neovide_padding_top = 0
vim.g.neovide_padding_bottom = 0
vim.g.neovide_padding_right = 0
vim.g.neovide_padding_left = 0

vim.g.neovide_floating_shadow = false
vim.g.neovide_floating_z_height = 10
vim.g.neovide_light_angle_degrees = 45
vim.g.neovide_light_radius = 5

vim.g.neovide_hide_mouse_when_typing = true

vim.g.neovide_cursor_trail_size = 0.2
vim.g.neovide_cursor_antialiasing = false
vim.g.neovide_cursor_animation_length = 0.1
vim.g.neovide_scroll_animation_length = 0.1

vim.keymap.set("v", "<D-c>", "\"+y")
vim.keymap.set("n", "<D-v>", "\"+P")
vim.keymap.set("v", "<D-v>", "\"+P")
vim.keymap.set("c", "<D-v>", "<C-R>+")
vim.keymap.set("i", "<D-v>", "<ESC>l\"+Pli")
