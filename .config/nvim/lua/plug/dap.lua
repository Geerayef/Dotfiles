return {
  "mfussenegger/nvim-dap",
  ft = { "cpp", "c", "python", "rust" },
  config = function()
    local dap = require("dap")
    local path_debugee = function()
      return vim.fn.input(
        "Path to executable: ",
        vim.fn.getcwd() .. "/",
        "file"
      )
    end
    -- Python ----------------------------------------------------------------------
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
    -- C|C++ -----------------------------------------------------------------------
    dap.adapters.gdb = {
      type = "executable",
      command = "gdb",
      args = { "-i", "dap" },
    }
    dap.adapters.codelldb = {
      type = "executable",
      command = os.getenv("HOME")
        .. "/software/codelldb/extension/adapter/codelldb",
    }
    dap.configurations.cpp = {
      {
        type = "codelldb",
        request = "launch",
        name = "[DAP] Launch CodeLLDB.",
        program = path_debugee,
        cwd = "${workspaceFolder}",
        stopOnEntry = false,
      },
      {
        type = "gdb",
        request = "launch",
        name = "[DAP] Launch GDB.",
        program = path_debugee,
        cwd = "${workspaceFolder}",
      },
    }
    dap.configurations.c = dap.configurations.cpp
    --------------------------------------------------------------------------------
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
