return {
  {
    dir = "plugins.zusline",
    name = "Zusline",
    event = "BufEnter",
    config = function ()
      local status, zus = pcall(require, "plugins.zusline.zus")
      if not status then
        print("~~~~~ [ERROR]: The Zus statusline could not be found.")
        return
      end
      zus.setup({})
    end
  },
  {
    dir = "plugins.zusline",
    name = "Zustabs",
    event = "BufEnter",
    config = function ()
      local status, tab = pcall(require, "plugins.zusline.tab")
      if not status then
        print("~~~~~ [ERROR]: The Zus tabline could not be found.")
        return
      end
      tab.setup({
        show_icons = true,
        show_name = false,
        show_modified_status = false,
      })
    end
  },
}
