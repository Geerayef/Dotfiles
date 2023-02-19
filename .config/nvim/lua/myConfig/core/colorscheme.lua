local status, github_theme = pcall(require, "github-theme")
if not status then
    print("Colorscheme not found!")
    return
end

github_theme.setup({
  theme_style = "dark_colorblind",
  comment_style = "italic",
  function_style = "italic",

  -- Change the "hint" color to the "orange" color, and make the "error" color bright red
  colors = {hint = "orange"},
})
