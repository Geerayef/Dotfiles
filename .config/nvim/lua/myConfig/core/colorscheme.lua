-- local status, onedark = pcall(require, "onedark")
-- if not status then
--     print("Colorscheme not found!")
--     return
-- end

-- folke/Tokyo Night config *Must come before calling the theme
-- tokyonight.setup {
--     style = "moon",
--     lualine_bold = true,
--     styles = {
--         keywords = { italic = false },
--     }
-- }

-- tiagovla/TokyoDark config
-- tokyodark.setup({
--     gamma = "1.1"
-- })


-- JoosepAlviste/palenightfall config
-- palenightfall.setup()

-- navarasu/onedark.nvim (deep, cool, darker)
-- onedark.setup({
--     style = "deep"
-- })
-- onedark.load()

vim.g.ayucolor = "dark"

vim.cmd("colorscheme ayu")
