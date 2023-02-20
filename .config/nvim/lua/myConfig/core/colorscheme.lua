local status, palenightfall = pcall(require, "palenightfall")
if not status then
    print("Colorscheme not found!")
    return
end

palenightfall.setup()
-- github_theme.setup({
--   theme_style = "dark",
--   comment_style = "italic",
--   function_style = "italic",
--
--   -- Change the "hint" color to the "orange" color, and make the "error" color bright red
--   colors = {hint = "orange"},
-- })
