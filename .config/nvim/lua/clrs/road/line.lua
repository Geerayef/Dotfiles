return {
  setup = function(palette)
    return {
      normal = {
        a = { bg = palette.dragonInk[100], fg = palette.lotusYellow[200] },
        b = { bg = palette.dragonInk[100], fg = palette.lotusYellow[200] },
        c = { bg = "Normal", fg = palette.charcoal[900], gui = "bold" },
      },
      insert = {
        a = { bg = palette.dragonInk[100], fg = palette.charcoal[900] },
        b = { bg = palette.dragonInk[100], fg = palette.charcoal[900] },
        c = { bg = "Normal", fg = palette.charcoal[900], gui = "bold" },
      },
      command = {
        a = { bg = palette.dragonInk[100], fg = palette.charcoal[900] },
        b = { bg = palette.dragonInk[100], fg = palette.charcoal[900] },
        c = { bg = "Normal", fg = palette.charcoal[900], gui = "bold" },
      },
      visual = {
        a = { bg = palette.dragonInk[100], fg = palette.charcoal[900] },
        b = { bg = palette.dragonInk[100], fg = palette.charcoal[900] },
        c = { bg = "Normal", fg = palette.charcoal[900], gui = "bold" },
      },
      replace = {
        a = { bg = palette.dragonInk[100], fg = palette.charcoal[900] },
        b = { bg = palette.dragonInk[100], fg = palette.charcoal[900] },
        c = { bg = "Normal", fg = palette.charcoal[900], gui = "bold" },
      },
      inactive = {
        a = { bg = "Normal", fg = palette.dragonInk[600] },
        b = { bg = "Normal", fg = palette.dragonInk[600] },
        c = { bg = "Normal", fg = palette.dragonInk[600] },
      },
    }
  end,
}
