---GRIM.func provides generic wrappers around Neovim's builtin functionality.
---@class GRIM.func
---@field notify fun(msg: string, lvl: string): nil
---@field filter fun(predicate: fun(x: any): boolean, t: table): table
---@field reduce fun(func: fun(x: any, ...: any): any, t: table, acc: any): any
---@field immutable fun(t: table): table

---@param msg string # Message to display
---@param lvl string # Log level
local notify = function(lvl, msg)
  if lvl == nil or msg == nil then return end
  local level = string.upper(lvl)
  vim.notify("[" .. level .. "] -- " .. msg, vim.log.levels[level])
end

---@param predicate fun(x: any): bool
---@param t table
---@return table
local filter = function(predicate, t)
  local new_tbl = {}
  for _i, v in ipairs(t) do
    if predicate(v) then table.insert(new_tbl, v) end
  end
  return new_tbl
end

---@generic T
---@param func fun(x: T, ...: any): T
---@param t table
---@param acc T
---@return T
local reduce = function(func, t, acc)
  local result = acc
  for _i, v in ipairs(t) do
    result = func(result, v)
  end
  return result
end

---Immutable table
---@param t table
---return table
local immutable = function(t)
  return setmetatable({}, {
    __index = t,
    __newindex = function() error("Attempt to modify immutable table") end,
  })
end

---@type GRIM.func
return {
  notify = notify,
  filter = filter,
  reduce = reduce,
  immutable = immutable,
}
