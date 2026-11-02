local notify = function(title, msg)
  vim.fn.system(string.format('notify-send "%s" "%s"', title, msg))
end

local tick = function(timer)
  notify("Work", "90min")
  timer:start(90 * 60 * 1000, 0, function()
    vim.schedule(function()
      notify("Break", "10min")
      timer:start(10 * 60 * 1000, 0, function()
        vim.schedule(function() tick(timer) end)
      end)
    end)
  end)
end

local init = function()
  local timer = vim.uv.new_timer()
  tick()
  return timer
end

-- Commands
vim.api.nvim_create_user_command("PomoStart", tick, {})
vim.api.nvim_create_user_command("PomoStop", function() timer:stop() end, {})
return { init = init }
