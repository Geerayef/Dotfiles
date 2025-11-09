return setmetatable({}, {
  __index = function(tbl, key)
    local ok, mod = pcall(require, "util." .. key)
    if ok then
      rawset(tbl, key, mod)
      return mod
    end
    return {}
  end,
})
