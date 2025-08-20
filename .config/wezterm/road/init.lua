local palette = require("road.palette")

local image = {
  ["Raisin black"] = "#292931",
  ["Jet"] = "#303039",
  ["Black olive"] = "#393A2E",
  ["Onyx"] = "#40414B",
  ["Ebony"] = "#565441",
  ["Payne's gray"] = "#5D646D",
  ["Cadet gray"] = "#8D9798",
  ["Sage"] = "#C0C4A8",
  ["Honeydew"] = "#D4E3D8",
  ["Straw"] = "#E1D77D",
}

---# Mint cream   "#D9E4DB",
---# Jet          "#373643",
---# Ebony        "#565848",
---# Cadet Gray   "#86909A",
---# Raisin Black "#2B2A30",
---# Citron       "#D7CC75",
---# Paynes Gray  "#5E686E",
---# Charcoal     "#4A515C",
---# Dragon Ink   "#010204"
---# Lotus Yellow "#FFF779"
---# Emerald      "#69DC9E"
---# Rusty Red    "#DA2F43"
---# Gunmetal     "#176087"
local base = {}
for k, v in pairs(palette) do
  base[k] = v["DEFAULT"]
end

return { base = base, palette = palette, image = image }
