return {
    dir = "plugins.statusline.zus",
    event = "BufEnter",
    config = function ()
        require("plugins.statusline.zus")
    end
}
