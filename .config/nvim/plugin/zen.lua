local status, zen = pcall(require, "true-zen")
if not status then
  return
end

zen.setup()
