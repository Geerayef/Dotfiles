local status, tokyonight = pcall(require, "tokyonight")
if not status then
    print("Colorscheme not found!")
    return
end

tokyonight.setup {
    style = "moon",
    lualine_bold = true,
    styles = {
        keywords = { italic = false },
    }
}

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

-- vim.g.ayucolor = "dark"

vim.cmd("colorscheme tokyonight")
