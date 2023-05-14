local status, tokyonight = pcall(require, "tokyonight")
if not status then
    print("Colorscheme not found!")
    return
end

-- Tokyo Night config *Must come before calling the theme
tokyonight.setup {
    style = "night"
}

vim.cmd('colorscheme tokyonight')

-- Ayu config
-- vim.g.ayu_mirage = true

-- TokyoDark config
-- vim.g.tokyodark_transparent_background = true
-- vim.g.tokyodark_color_gamma = "1.1"

-- PaleNightFall config
-- palenightfall.setup()

-- GitHug config
-- github_theme.setup({
--   theme_style = "dark",
--   comment_style = "italic",
--   function_style = "italic",
--
--   -- Change the "hint" color to the "orange" color, and make the "error" color bright red
--   colors = {hint = "orange"},
-- })
