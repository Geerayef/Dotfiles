return {
    {
        dir = "plugins.zusline.zus",
        event = "BufEnter",
        config = function ()
            local zus = require("plugins.zusline.zus")
            zus.setup({})
        end
    },
    {
        dir = "plugins.zusline.tab",
        event = "BufEnter",
        config = function ()
            local tab = require("plugins.zusline.tab")
            tab.setup({
                show_name = false,
                show_modified_status = false,
                show_icon = true,
            })
        end
    },
}
