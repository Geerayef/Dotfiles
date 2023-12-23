-- local theme = {
--   diag = {
--     error = "#E82424",
--     hint = "#6A9589",
--     info = "#658594",
--     ok = "#98BB6C",
--     warning = "#FF9E3B"
--   },
--   diff = {
--     add = "#2B3328",
--     change = "#252535",
--     delete = "#43242B",
--     text = "#49443C"
--   },
--   syn = {
--     comment = "#737c73",
--     constant = "#b6927b",
--     deprecated = "#717C7C",
--     fun = "#8ba4b0",
--     identifier = "#c4b28a",
--     keyword = "#8992a7",
--     number = "#a292a3",
--     operator = "#c4746e",
--     parameter = "#a6a69c",
--     preproc = "#c4746e",
--     punct = "#9e9b93",
--     regex = "#c4746e",
--     special1 = "#949fb5",
--     special2 = "#c4746e",
--     special3 = "#c4746e",
--     statement = "#8992a7",
--     string = "#8a9a7b",
--     type = "#8ea4a2",
--     variable = "none"
--   },
--   term = {
--     "#0D0C0C",
--     "#C4746E",
--     "#8A9A7B",
--     "#C4B28A",
--     "#8BA4B0",
--     "#A292A3",
--     "#8EA4A2",
--     "#C8C093",
--     "#A6A69C",
--     "#E46876",
--     "#87A987",
--     "#E6C384",
--     "#7FB4CA",
--     "#938AA9",
--     "#7AA89F",
--     "#C5C9C5",
--     "#B6927B",
--     "#B98D7B"
--   },
--   ui = {
--     bg = "#181616",
--     bg_dim = "#12120f",
--     bg_gutter = "none",
--     bg_m1 = "#1D1C19",
--     bg_m2 = "#12120f",
--     bg_m3 = "#0d0c0c",
--     bg_p1 = "#282727",
--     bg_p2 = "#393836",
--     bg_search = "#2D4F67",
--     bg_visual = "#223249",
--     fg = "#c5c9c5",
--     fg_dim = "#C8C093",
--     fg_reverse = "#223249",
--     float = {
--       bg = "#0d0c0c",
--       bg_border = "#0d0c0c",
--       fg = "#C8C093",
--       fg_border = "#54546D"
--     },
--     nontext = "#625e5a",
--     pmenu = {
--       bg = "#223249",
--       bg_sbar = "#223249",
--       bg_sel = "#2D4F67",
--       bg_thumb = "#2D4F67",
--       fg = "#DCD7BA",
--       fg_sel = "none"
--     },
--     special = "#7a8382",
--     whitespace = "#625e5a"
--   },
--   vcs = {
--     added = "#76946A",
--     changed = "#DCA561",
--     removed = "#C34043"
--   }
-- }
--
-- local palette = {
--   autumnGreen = "#76946A",
--   autumnRed = "#C34043",
--   autumnYellow = "#DCA561",
--   boatYellow1 = "#938056",
--   boatYellow2 = "#C0A36E",
--   carpYellow = "#E6C384",
--   crystalBlue = "#7E9CD8",
--   dragonAqua = "#8ea4a2",
--   dragonAsh = "#737c73",
--   dragonBlack0 = "#0d0c0c",
--   dragonBlack1 = "#12120f",
--   dragonBlack2 = "#1D1C19",
--   dragonBlack3 = "#181616",
--   dragonBlack4 = "#282727",
--   dragonBlack5 = "#393836",
--   dragonBlack6 = "#625e5a",
--   dragonBlue = "#658594",
--   dragonBlue2 = "#8ba4b0",
--   dragonGray = "#a6a69c",
--   dragonGray2 = "#9e9b93",
--   dragonGray3 = "#7a8382",
--   dragonGreen = "#87a987",
--   dragonGreen2 = "#8a9a7b",
--   dragonOrange = "#b6927b",
--   dragonOrange2 = "#b98d7b",
--   dragonPink = "#a292a3",
--   dragonRed = "#c4746e",
--   dragonTeal = "#949fb5",
--   dragonViolet = "#8992a7",
--   dragonWhite = "#c5c9c5",
--   dragonYellow = "#c4b28a",
--   fujiGray = "#727169",
--   fujiWhite = "#DCD7BA",
--   katanaGray = "#717C7C",
--   lightBlue = "#A3D4D5",
--   lotusAqua = "#597b75",
--   lotusAqua2 = "#5e857a",
--   lotusBlue1 = "#c7d7e0",
--   lotusBlue2 = "#b5cbd2",
--   lotusBlue3 = "#9fb5c9",
--   lotusBlue4 = "#4d699b",
--   lotusBlue5 = "#5d57a3",
--   lotusCyan = "#d7e3d8",
--   lotusGray = "#dcd7ba",
--   lotusGray2 = "#716e61",
--   lotusGray3 = "#8a8980",
--   lotusGreen = "#6f894e",
--   lotusGreen2 = "#6e915f",
--   lotusGreen3 = "#b7d0ae",
--   lotusInk1 = "#545464",
--   lotusInk2 = "#43436c",
--   lotusOrange = "#cc6d00",
--   lotusOrange2 = "#e98a00",
--   lotusPink = "#b35b79",
--   lotusRed = "#c84053",
--   lotusRed2 = "#d7474b",
--   lotusRed3 = "#e82424",
--   lotusRed4 = "#d9a594",
--   lotusTeal1 = "#4e8ca2",
--   lotusTeal2 = "#6693bf",
--   lotusTeal3 = "#5a7785",
--   lotusViolet1 = "#a09cac",
--   lotusViolet2 = "#766b90",
--   lotusViolet3 = "#c9cbd1",
--   lotusViolet4 = "#624c83",
--   lotusWhite0 = "#d5cea3",
--   lotusWhite1 = "#dcd5ac",
--   lotusWhite2 = "#e5ddb0",
--   lotusWhite3 = "#f2ecbc",
--   lotusWhite4 = "#e7dba0",
--   lotusWhite5 = "#e4d794",
--   lotusYellow = "#77713f",
--   lotusYellow2 = "#836f4a",
--   lotusYellow3 = "#de9800",
--   lotusYellow4 = "#f9d791",
--   oldWhite = "#C8C093",
--   oniViolet = "#957FB8",
--   oniViolet2 = "#b8b4d0",
--   peachRed = "#FF5D62",
--   roninYellow = "#FF9E3B",
--   sakuraPink = "#D27E99",
--   samuraiRed = "#E82424",
--   springBlue = "#7FB4CA",
--   springGreen = "#98BB6C",
--   springViolet1 = "#938AA9",
--   springViolet2 = "#9CABCA",
--   sumiInk0 = "#16161D",
--   sumiInk1 = "#181820",
--   sumiInk2 = "#1a1a22",
--   sumiInk3 = "#1F1F28",
--   sumiInk4 = "#2A2A37",
--   sumiInk5 = "#363646",
--   sumiInk6 = "#54546D",
--   surimiOrange = "#FFA066",
--   waveAqua1 = "#6A9589",
--   waveAqua2 = "#7AA89F",
--   waveBlue1 = "#223249",
--   waveBlue2 = "#2D4F67",
--   waveRed = "#E46876",
--   winterBlue = "#252535",
--   winterGreen = "#2B3328",
--   winterRed = "#43242B",
--   winterYellow = "#49443C"
-- }

local theme = require("kanagawa.colors").setup({ theme = "dragon" }).theme

local kanagawa = {}

kanagawa.normal = {
  a = { bg = theme.syn.fun, fg = theme.ui.bg_m3 },
  b = { bg = theme.diff.change, fg = theme.syn.fun },
  c = { bg = theme.ui.bg_p1, fg = theme.ui.fg },
}

kanagawa.insert = {
  a = { bg = theme.diag.ok, fg = theme.ui.bg },
  b = { bg = theme.ui.bg, fg = theme.diag.ok },
}

kanagawa.command = {
  a = { bg = theme.syn.operator, fg = theme.ui.bg },
  b = { bg = theme.ui.bg, fg = theme.syn.operator },
}

kanagawa.visual = {
  a = { bg = theme.syn.keyword, fg = theme.ui.bg },
  b = { bg = theme.ui.bg, fg = theme.syn.keyword },
}

kanagawa.replace = {
  a = { bg = theme.syn.constant, fg = theme.ui.bg },
  b = { bg = theme.ui.bg, fg = theme.syn.constant },
}

kanagawa.inactive = {
  a = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
  b = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim, gui = "bold" },
  c = { bg = theme.ui.bg_m3, fg = theme.ui.fg_dim },
}

if vim.g.kanagawa_lualine_bold then
  for _, mode in pairs(kanagawa) do
    mode.a.gui = "bold"
  end
end

return kanagawa
