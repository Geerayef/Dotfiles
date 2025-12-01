---Wrapper for `nvim_get_hl()`.
---Does not create a highlight group if it doesn't exist.
---(Default: `opts.create = false`).
---`opts.winhl_link`: Get highlight attributes without effect of `winhl`
---@param ns_id integer
---@param opts { name: string?, id: integer?, link: boolean?, create: boolean?, [any]: any }
---@return vim.api.keyset.hl_info # Highlight attributes
local get = function(ns_id, opts)
  local no_winhl_link = opts.winhl_link == false
  opts.winhl_link = nil
  opts.create = opts.create or false
  local attr = vim.api.nvim_get_hl(ns_id, opts)
  if no_winhl_link then
    while attr.link do
      opts.name = attr.link
      attr = vim.api.nvim_get_hl(ns_id, opts)
    end
  end
  return attr
end

---Wrapper for `nvim_buf_add_highlight()`.
---Does not create a cleared highlight group if it doesn't exist.
---@param buf integer # Buffer handle
---@param ns_id integer # Namespace to use (-1: ungrouped highlight)
---@param hl_group string # Highlight group name
---@param line integer # 0-indexed line number
---@param col_start integer # Start of (byte-indexed) column range to highlight
---@param col_end integer # End of (byte-indexed) column range to highlight (-1: highlight to the end of line)
---@return nil
local buf_add = function(buf, ns_id, hl_group, line, col_start, col_end)
  if vim.fn.hlexists(hl_group) == 0 then return end
  vim.api.nvim_buf_add_highlight(buf, ns_id, hl_group, line, col_start, col_end)
end

---Highlight text in buffer, clear previous highlight if any exists.
---@param buf integer
---@param hlgroup string
-- TODO: What to do with this? ---@param range winbar_symbol_range_t?
local range_single = function(buf, hlgroup, range)
  if not vim.api.nvim_buf_is_valid(buf) then return end
  local ns = vim.api.nvim_create_namespace(hlgroup)
  vim.api.nvim_buf_clear_namespace(buf, ns, 0, -1)
  if range then
    for linenr = range.start.line, range["end"].line do
      local start_col = linenr == range.start.line and range.start.character
        or 0
      local end_col = linenr == range["end"].line and range["end"].character
        or -1
      HL.buf_add(buf, ns, hlgroup, linenr, start_col, end_col)
    end
  end
end

---Highlight a line in buffer, clear previous highlight if any exists.
---@param buf integer
---@param hlgroup string
---@param line integer? # 1-indexed line number
local line_single = function(buf, hlgroup, line)
  HL.range_single(buf, hlgroup, line and {
    start = { line = line - 1, character = 0 },
    ["end"] = { line = line - 1, character = -1 },
  })
end

---Merge highlight attributes.
---In case of conflict use values from the right most highlight group.
---@vararg string # Highlight group names
---@return vim.api.keyset.highlight # Merged highlight attributes
local merge = function(...)
  -- Eliminate nil values in vararg
  local hl_names = {}
  for _, hl_name in pairs({ ... }) do
    if hl_name then table.insert(hl_names, hl_name) end
  end
  local hl_attr = vim.tbl_map(
    function(hl_name)
      return HL.get(0, {
        name = hl_name,
        winhl_link = false,
      })
    end,
    hl_names
  )
  return vim.tbl_extend("force", unpack(hl_attr))
end

---@param attr_type 'fg'|'bg'|'ctermfg'|'ctermbg'
---@param fbg? string|integer
---@param default? integer
---@return integer|string|nil
local normalize_fg_or_bg = function(attr_type, fbg, default)
  if not fbg then return default end
  local data_type = type(fbg)
  if data_type == "number" then
    if attr_type:match("^cterm") then
      return fbg >= 0 and fbg <= 255 and fbg or default
    end
    return fbg
  end
  if data_type == "string" then
    if vim.fn.hlexists(fbg) == 1 then
      return HL.get(0, {
        name = fbg,
        winhl_link = false,
      })[attr_type]
    end
    if fbg:match("^#%x%x%x%x%x%x$") then
      if attr_type:match("^cterm") then return default end
      return fbg
    end
  end
  return default
end

---Normalize highlight attributes.
---1. Replace `attr.fg` and `attr.bg` with their corresponding color codes
---   if they are set to highlight group names
---2. If `attr.link` used in combination with other attributes, will first
---   retrieve the attributes of the linked highlight group, then merge
---   with other attributes
---Side effect: change `attr` table
---@param attr vim.api.keyset.highlight # Highlight attributes
---@return table # Normalized highlight attributes
local normalize = function(attr)
  if attr.link then
    local num_keys = #vim.tbl_keys(attr)
    if num_keys <= 1 then return attr end
    attr.fg = HL.normalize_fg_or_bg("fg", attr.fg)
    attr.bg = HL.normalize_fg_or_bg("bg", attr.bg)
    attr = vim.tbl_extend("force", HL.get(0, {
      name = attr.link,
      winhl_link = false,
    }) or {}, attr)
    attr.link = nil
    return attr
  end
  local fg = attr.fg
  local bg = attr.bg
  local ctermfg = attr.ctermfg
  local ctermbg = attr.ctermbg
  attr.fg = HL.normalize_fg_or_bg("fg", fg)
  attr.bg = HL.normalize_fg_or_bg("bg", bg)
  attr.ctermfg = HL.normalize_fg_or_bg("ctermfg", ctermfg or fg)
  attr.ctermbg = HL.normalize_fg_or_bg("ctermbg", ctermbg or bg)
  return attr
end

---Wrapper for `nvim_set_hl()`.
---Normalize highlight attributes before setting.
---@param ns_id integer
---@param name string
---@param attr vim.api.keyset.highlight # Highlight attributes
---@return nil
local set = function(ns_id, name, attr)
  vim.api.nvim_set_hl(ns_id, name, HL.normalize(attr))
end

---Set default highlight attributes, normalize highlight attributes before setting.
---@param ns_id integer
---@param name string
---@param attr vim.api.keyset.highlight # Highlight attributes
---@return nil
local set_default = function(ns_id, name, attr)
  attr.default = true
  return vim.api.nvim_set_hl(ns_id, name, HL.normalize(attr))
end

local todec = {
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
  while digit <= #hex do
    dec = dec + todec[string.sub(hex, digit, digit)] * 16 ^ (#hex - digit)
    digit = digit + 1
  end
  return dec
end

---Convert decimal to hexadecimal.
---@param int integer
---@param n_digits integer? # Number of digits used for the hex code
---@return string hex
local dec2hex = function(int, n_digits)
  return not n_digits and string.format("%x", int)
    or string.format("%0" .. n_digits .. "x", int)
end

---Convert hex to rgb.
---@param hex string # hex code
---@return integer[] rgb
local hex2rgb = function(hex)
  return {
    HL.hex2dec(string.sub(hex, 1, 2)),
    HL.hex2dec(string.sub(hex, 3, 4)),
    HL.hex2dec(string.sub(hex, 5, 6)),
  }
end

---Convert rgb to hex.
---@param rgb integer[]
---@return string
local rgb2hex = function(rgb)
  local hex = {
    HL.dec2hex(math.floor(rgb[1])),
    HL.dec2hex(math.floor(rgb[2])),
    HL.dec2hex(math.floor(rgb[3])),
  }
  hex = {
    string.rep("0", 2 - #hex[1]) .. hex[1],
    string.rep("0", 2 - #hex[2]) .. hex[2],
    string.rep("0", 2 - #hex[3]) .. hex[3],
  }
  return table.concat(hex, "")
end

---Blend two colors.
---@param c1 number|string|table # The first color, in hex, dec, or rgb
---@param c2 number|string|table # The second color, in hex, dec, or rgb
---@param alpha number? # Weight of the first color [0-1] (Default: 0.5)
---@return { hex: string, dec: integer, r: integer, g: integer, b: integer }
local cblend = function(c1, c2, alpha)
  alpha = alpha or 0.5
  c1 = type(c1) == "number" and HL.dec2hex(c1, 6) or c1
  c2 = type(c2) == "number" and HL.dec2hex(c2, 6) or c2
  local rgb1 = type(c1) == "string" and HL.hex2rgb(c1:gsub("#", "", 1)) or c1
  local rgb2 = type(c2) == "string" and HL.hex2rgb(c2:gsub("#", "", 1)) or c2
  local rgb_blended = {
    alpha * rgb1[1] + (1 - alpha) * rgb2[1],
    alpha * rgb1[2] + (1 - alpha) * rgb2[2],
    alpha * rgb1[3] + (1 - alpha) * rgb2[3],
  }
  local hex = HL.rgb2hex(rgb_blended)
  return {
    hex = "#" .. hex,
    dec = HL.hex2dec(hex),
    r = math.floor(rgb_blended[1]),
    g = math.floor(rgb_blended[2]),
    b = math.floor(rgb_blended[3]),
  }
end

---Blend two hlgroups.
---@param h1 string|table # The first hlgroup name or highlight attribute table
---@param h2 string|table # The second hlgroup name or highlight attribute table
---@param alpha number? # Weight of the first color [0-1] (Default: 0.5)
---@return table # Merged color or highlight attributes
local blend = function(h1, h2, alpha)
  h1 = type(h1) == "table" and h1
    or HL.get(0, { name = h1, winhl_link = false })
  h2 = type(h2) == "table" and h2
    or HL.get(0, { name = h2, winhl_link = false })
  local fg = h1.fg and h2.fg and HL.cblend(h1.fg, h2.fg, alpha).dec
    or h1.fg
    or h2.fg
  local bg = h1.bg and h2.bg and HL.cblend(h1.bg, h2.bg, alpha).dec
    or h1.bg
    or h2.bg
  return vim.tbl_deep_extend("force", h1, h2, { fg = fg, bg = bg })
end

HL = {
  get = get,
  buf_add = buf_add,
  range_single = range_single,
  line_single = line_single,
  merge = merge,
  normalize_fg_or_bg = normalize_fg_or_bg,
  normalize = normalize,
  set = set,
  set_default = set_default,
  hex2dec = hex2dec,
  dec2hex = dec2hex,
  hex2rgb = hex2rgb,
  rgb2hex = rgb2hex,
  cblend = cblend,
  blend = blend,
}
return HL
