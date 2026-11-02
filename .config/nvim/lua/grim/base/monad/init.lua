---@class GRIM.base.monad
---@field chain GRIM.base.monad.Chain
---@field either GRIM.base.monad.Either
---@field maybe GRIM.base.monad.Maybe
return {
  chain = require("grim.base.monad.chain"),
  maybe = require("grim.base.monad.maybe"),
  either = require("grim.base.monad.either"),
}
