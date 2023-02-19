local status, neodev = pcall(require, "neodev")
if not status then
    return
end

neodev.setup()
