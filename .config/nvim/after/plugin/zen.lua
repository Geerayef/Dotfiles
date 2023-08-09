local status, zen = pcall(require, "true-zen")
if not status then
  return
end

zen.setup({
  modes = {
    ataraxis = {
      minimum_writing_area = {
        width = 70,
        height = 45
      }
    }
  }
})
