return {
  {
    dir = "plugins.zusline.zus",
    event = "BufEnter",
    config = function ()
      local status, zus = pcall(require, "plugins.zusline.zus")
      if not status then
        print("~~~~~ [ERROR]: The file zus.lua could not be found.")
        return
      end
      zus.setup({})
    end
  },
  {
    dir = "plugins.zusline.tab",
    event = "BufEnter",
    config = function ()
      local status, tab = pcall(require, "plugins.zusline.tab")
      if not status then
        print("~~~~~ [ERROR]: The file tab.lua could not be found.")
        return
      end
      tab.setup({
        show_name = false,
        show_modified_status = false,
        show_icon = true,
      })
    end
  },
}
