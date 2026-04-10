---Wrapper for `nvim_get_hl()`.
---Does not create a highlight group if it doesn't exist.
---(Default: `opts.create = false`).
---`opts.winhl_link`: Get highlight attributes without effect of `winhl`
---@param nsid integer # Namespace ID
---@param opts vim.api.keyset.get_highlight
---@return vim.api.keyset.get_hl_info # Highlight attributes
local get = function(nsid, opts)
  local link_p = opts.link == false
  opts.link = nil
  opts.create = opts.create or false
  local hlinfo = vim.api.nvim_get_hl(nsid, opts)
  if link_p then
    while hlinfo.link do
      opts.name = hlinfo.link
      hlinfo = vim.api.nvim_get_hl(nsid, opts)
    end
  end
  return hlinfo
end

---Highlight text in buffer, clear previous highlight if any exists.
---@param bufid integer # Buffer ID
---@param hlid string # Highlight group ID
---@param range { start: [ integer, integer ], finish: [ integer, integer ] } # Range is two line-column pairs
---@return nil
local apply = function(bufid, hlid, range)
  if not vim.api.nvim_buf_is_valid(bufid) then return end
  if range and type(range) ~= "table" then return end
  if not (range.start and range.finish) then return end
  if not (#range.start ~= 2 and #range.finish ~= 2) then return end
  local nsid = vim.api.nvim_create_namespace(hlid)
  vim.api.nvim_buf_clear_namespace(bufid, nsid, 0, -1)
  vim.hl.range(bufid, nsid, hlid, range.start, range.finish)
end

---Merge highlight attributes.
---In case of conflict use values from the right most highlight group.
---@vararg string # Highlight group names
---@return vim.api.keyset.highlight # Merged highlight attributes
local merge = function(...)
  local hlids = vim.tbl_filter(function(e) return e ~= nil end, { ... })
  local hl_attributes = vim.tbl_map(
    function(hlid) return get(0, { name = hlid, link = false }) end,
    hlids
  )
  return vim.tbl_extend("force", unpack(hl_attributes))
end

---@param attribute "fg"|"bg"|"ctermfg"|"ctermbg"
---@param fbg? string|integer # Fore/Back ground
---@param default? integer
---@return integer|string|nil
local attribute_normalise = function(attribute, fbg, default)
  if not fbg then return default end
  local type_fbg = type(fbg)
  if type_fbg == "number" then
    if attribute:match("^cterm") then return fbg >= 0 and fbg <= 255 and fbg or default end
    return fbg
  end
  if type_fbg == "string" then
    if vim.fn.hlexists(fbg) == 1 then return get(0, { name = fbg, link = false })[attribute] end
    if fbg:match("^#%x%x%x%x%x%x$") and attribute:match("^cterm") then return default end
    return fbg
  end
  return default
end

---Normalise highlight attributes.
---@param attribute vim.api.keyset.highlight
---@return table
local normalise = function(attribute)
  if attribute.link then
    if #vim.tbl_keys(attribute) < 2 then return attribute end
    attribute.fg = attribute_normalise("fg", attribute.fg)
    attribute.bg = attribute_normalise("bg", attribute.bg)
    local hl_linked = type(attribute.link) == "string" and { name = attribute.link, link = false }
      or { id = attribute.link, link = false }
    attribute = vim.tbl_extend("force", get(0, hl_linked) or {}, attribute)
    attribute.link = nil
    return attribute
  end
  attribute.fg = attribute_normalise("fg", attribute.fg)
  attribute.bg = attribute_normalise("bg", attribute.bg)
  attribute.ctermfg = attribute_normalise("ctermfg", attribute.ctermfg or attribute.fg)
  attribute.ctermbg = attribute_normalise("ctermbg", attribute.ctermbg or attribute.bg)
  return attribute
end

---Wrapper for `nvim_set_hl()`.
---@param nsid integer
---@param name string
---@param attributes vim.api.keyset.highlight # Highlight attributes
---@return nil
local set = function(nsid, name, attributes) vim.api.nvim_set_hl(nsid, name, normalise(attributes)) end

---Set default highlight attributes, normalise highlight attributes before setting.
---@param nsid integer
---@param name string
---@param attribute vim.api.keyset.highlight # Highlight attributes
---@return nil
local set_default = function(nsid, name, attribute)
  attribute.default = true
  return vim.api.nvim_set_hl(nsid, name, normalise(attribute))
end

local hex2decimal = {
  ["0"] = 0,
  ["1"] = 1,
  ["2"] = 2,
  ["3"] = 3,
  ["4"] = 4,
  ["5"] = 5,
  ["6"] = 6,
  ["7"] = 7,
  ["8"] = 8,
  ["9"] = 9,
  ["a"] = 10,
  ["b"] = 11,
  ["c"] = 12,
  ["d"] = 13,
  ["e"] = 14,
  ["f"] = 15,
  ["A"] = 10,
  ["B"] = 11,
  ["C"] = 12,
  ["D"] = 13,
  ["E"] = 14,
  ["F"] = 15,
}

---Convert hexadecimal to decimal.
---@param hex string
---@return integer dec
local hex2dec = function(hex)
  local digit = 1
  local dec = 0
  local lenhex = #hex
  while digit <= #hex do
    dec = dec + hex2decimal[string.sub(hex, digit, digit)] * 16 ^ (lenhex - digit)
    digit = digit + 1
  end
  return dec
end

---Convert decimal to hexadecimal.
---@param int integer
---@param n_digits integer? # Number of digits used for the hex code
---@return string hex
local dec2hex = function(int, n_digits)
  return not n_digits and string.format("%x", int) or string.format("%0" .. n_digits .. "x", int)
end

---Convert hex to rgb.
---@param hex string # hex code
---@return integer[] rgb
local hex2rgb = function(hex)
  return {
    hex2dec(string.sub(hex, 1, 2)),
    hex2dec(string.sub(hex, 3, 4)),
    hex2dec(string.sub(hex, 5, 6)),
  }
end

---Convert rgb to hex.
---@param rgb integer[]
---@return string
local rgb2hex = function(rgb)
  local hex = {
    dec2hex(math.floor(rgb[1])),
    dec2hex(math.floor(rgb[2])),
    dec2hex(math.floor(rgb[3])),
  }
  hex = {
    string.rep("0", 2 - #hex[1]) .. hex[1],
    string.rep("0", 2 - #hex[2]) .. hex[2],
    string.rep("0", 2 - #hex[3]) .. hex[3],
  }
  return table.concat(hex, "")
end

---Blend two colors.
---@param c1 number|string|table
---@param c2 number|string|table
---@param alpha number?
---@return { hex: string, dec: integer, r: integer, g: integer, b: integer }
local cblend = function(c1, c2, alpha)
  alpha = alpha or 0.5
  c1 = type(c1) == "number" and dec2hex(c1, 6) or c1
  c2 = type(c2) == "number" and dec2hex(c2, 6) or c2
  local rgb1 = type(c1) == "string" and hex2rgb(c1:gsub("#", "", 1)) or c1
  local rgb2 = type(c2) == "string" and hex2rgb(c2:gsub("#", "", 1)) or c2
  local rgb_blended = {
    alpha * rgb1[1] + (1 - alpha) * rgb2[1],
    alpha * rgb1[2] + (1 - alpha) * rgb2[2],
    alpha * rgb1[3] + (1 - alpha) * rgb2[3],
  }
  local hex = rgb2hex(rgb_blended)
  return {
    hex = "#" .. hex,
    dec = hex2dec(hex),
    r = math.floor(rgb_blended[1]),
    g = math.floor(rgb_blended[2]),
    b = math.floor(rgb_blended[3]),
  }
end

---Blend two highlight groups.
---@param h1 string|table
---@param h2 string|table
---@param alpha number?
---@return table
local blend = function(h1, h2, alpha)
  h1 = type(h1) == "table" and h1 or get(0, { name = h1, winhl_link = false })
  h2 = type(h2) == "table" and h2 or get(0, { name = h2, winhl_link = false })
  local fg = h1.fg and h2.fg and cblend(h1.fg, h2.fg, alpha).dec or h1.fg or h2.fg
  local bg = h1.bg and h2.bg and cblend(h1.bg, h2.bg, alpha).dec or h1.bg or h2.bg
  return vim.tbl_deep_extend("force", h1, h2, { fg = fg, bg = bg })
end

---@class hl
---@field get fun()
---@field buf_add fun()
---@field range_single fun()
---@field line_single fun()
---@field merge fun()
---@field attribute_normalise fun()
---@field normalise fun()
---@field set fun()
---@field set_default fun()
---@field hex2dec fun()
---@field dec2hex fun()
---@field hex2rgb fun()
---@field rgb2hex fun()
---@field cblend fun()
---@field blend fun()
return {
  get = get,
  apply = apply,
  merge = merge,
  attribute_normalise = attribute_normalise,
  normalize = normalise,
  set = set,
  set_default = set_default,
  hex2dec = hex2dec,
  dec2hex = dec2hex,
  hex2rgb = hex2rgb,
  rgb2hex = rgb2hex,
  cblend = cblend,
  blend = blend,
}
