return {
  "mfussenegger/nvim-dap",
  event = "VeryLazy",
  config = function()
    local dap = require("dap")
    dap.adapters.python = function(cb, config)
      if config.request == "attach" then
        local port = (config.connect or config).port
        local host = (config.connect or config).host or "127.0.0.1"
        cb({
          type = "server",
          port = assert(
            port,
            "`connect.port` is required for a python `attach` configuration"
          ),
          host = host,
          options = { source_filetype = "python" },
        })
      else
        -- config.request == "launch"
        cb({
          type = "executable",
          command = "python",
          args = { "-m", "debugpy.adapter" },
          options = { source_filetype = "python" },
        })
      end
    end
    dap.configurations.python = {
      {
        type = "python",
        request = "attach",
        name = "[DAP] Attach to a Python debugger.",
        connect = function()
          local host = vim.fn.input("Host: ")
          local port = tonumber(vim.fn.input("Port: "))
          return { host = host, port = port }
        end,
      },
      {
        type = "python",
        request = "launch",
        name = "[DAP] Launch Python debugger.",
        -- Options below are for debugpy, see https://github.com/microsoft/debugpy/wiki/Debug-configuration-settings
        program = "${file}",
        pythonPath = function()
          if
            os.getenv("VIRTUAL_ENV") ~= nil
            and os.getenv("PYENV_VIRTUAL_ENV") ~= nil
          then
            return (os.getenv("PYENV_VIRTUAL_ENV") .. "/bin/python")
          elseif os.getenv("CONDA_PREFIX") ~= nil then
            return (os.getenv("CONDA_PREFIX") .. "/bin/python")
          else
            return "python"
          end
        end,
      },
    }
    vim.fn.sign_define("DapBreakpoint", {
      text = S.Icons.ui.circle_circle,
      texthl = "DiagnosticError",
      numhl = "",
    })
    vim.fn.sign_define("DapStopped", {
      text = S.Icons.ui.angle_right_l,
      texthl = "DiagnosticHint",
      numhl = "",
    })
  end,
}
