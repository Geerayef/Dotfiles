return setmetatable({}, {
  __index = function(self, key)
    self[key] = require("util." .. key)
    return self[key]
  end,
})
