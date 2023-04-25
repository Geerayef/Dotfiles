local tokyodark, status = pcall(require, "tokyodark")
if not status then
    print("Colorscheme not found!")
    return
end

vim.cmd("colorscheme tokyodark")

-- vim.g.ayu_mirage = true

-- vim.g.tokyodark_transparent_background = true
vim.g.tokyodark_color_gamma = "1.1"

-- palenightfall.setup()

-- github_theme.setup({
--   theme_style = "dark",
--   comment_style = "italic",
--   function_style = "italic",
--
--   -- Change the "hint" color to the "orange" color, and make the "error" color bright red
--   colors = {hint = "orange"},
-- })
