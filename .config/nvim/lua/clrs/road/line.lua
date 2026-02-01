return {
  setup = function(palette)
    return {
      normal = {
        a = { bg = palette.dragonInk["DEFAULT"], fg = palette.lotusYellow[200] },
        b = { bg = palette.dragonInk["DEFAULT"], fg = palette.lotusYellow[200] },
        c = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900], gui = "bold" },
      },
      insert = {
        a = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900] },
        b = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900] },
        c = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900], gui = "bold" },
      },
      command = {
        a = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900] },
        b = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900] },
        c = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900], gui = "bold" },
      },
      visual = {
        a = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900] },
        b = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900] },
        c = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900], gui = "bold" },
      },
      replace = {
        a = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900] },
        b = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900] },
        c = { bg = palette.dragonInk["DEFAULT"], fg = palette.charcoal[900], gui = "bold" },
      },
      inactive = {
        a = { bg = palette.dragonInk["DEFAULT"], fg = palette.dragonInk[600] },
        b = { bg = palette.dragonInk["DEFAULT"], fg = palette.dragonInk[600] },
        c = { bg = palette.dragonInk["DEFAULT"], fg = palette.dragonInk[600] },
      },
    }
  end,
}
