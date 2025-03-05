LSP = {}

---@class ActiveClient
---@field id integer # LSP client ID
---@field name string # LSP client name

---@type ActiveClient[]
LSP.active_clients = {}

local ok_cmp, cmplsp = pcall(require, "cmp_nvim_lsp")
if not ok_cmp then F.Notify("info", "`cmp_nvim_lsp` not found.") end
local caps = vim.tbl_deep_extend(
  "force",
  {},
  {
    workspace = { didChangeWatchedFiles = { dynamicRegistration = true } },
    textDocument = {
      documentFormattingProvider = false,
      codelens = { enable = true },
      completion = {
        completionItem = {
          snippetSupport = true,
          resolveSupport = {
            properties = { "detail", "documentation", "additionalTextEdits" },
          },
        },
      },
    },
  },
  ok_cmp
      and cmplsp.default_capabilities(
        vim.lsp.protocol.make_client_capabilities()
      )
    or vim.lsp.protocol.make_client_capabilities()
)

---@class vim.lsp.ClientConfig: lsp_client_config_t
---@class lsp_client_config_t
---@field cmd? (string[]|fun(dispatchers: table):table)
---@field cmd_cwd? string
---@field cmd_env? (table)
---@field detached? boolean
---@field workspace_folders? (table)
---@field capabilities? lsp.ClientCapabilities
---@field handlers? table<string,function>
---@field settings? table
---@field commands? table
---@field init_options? table
---@field name? string
---@field get_language_id? fun(bufnr: integer, filetype: string): string
---@field offset_encoding? string
---@field on_error? fun(code: integer)
---@field before_init? function
---@field on_init? function
---@field on_exit? fun(code: integer, signal: integer, client_id: integer)
---@field on_attach? fun(client: vim.lsp.Client, bufnr: integer)
---@field trace? 'off'|'messages'|'verbose'|nil
---@field flags? table
---@field root_dir? string
---@field root_patterns? string[]

LSP.default = {
  root_patterns = S.root_markers,
  ---@type lsp_client_config_t
  config = {
    capabilities = caps,
    single_file_support = true,
  },
}

---Wrapper for `vim.lsp.start()`.
---Starts and attaches LSP client to the current buffer.
---@param config vim.lsp.ClientConfig -- lsp_client_config_t
---@param opts table? # Options passed to the `vim.lsp.start()` function
---@return integer? client_id # ID of attached client or nil if failed
function LSP.start(config, opts)
  if
    vim.b.bigfile
    or vim.bo.bt == "nofile"
    or type(config.cmd) ~= "table"
    or vim.fn.executable(config.cmd[1]) == 0
  then
    return nil
  end
  local client_id = nil
  do
    client_id = vim.lsp.start(
      vim.tbl_deep_extend("keep", config or {}, {
        name = config.cmd[1],
        root_dir = require("util.fs").root(
          vim.api.nvim_buf_get_name(0),
          vim.list_extend(LSP.default.root_patterns, config.root_patterns)
        ),
      }, LSP.default.config),
      opts
    )
    if client_id ~= nil then
      if
        not vim.tbl_contains(
          LSP.active_clients,
          function(e) return e[2] == config.cmd[1] end,
          { predicate = true }
        )
      then
        table.insert(LSP.active_clients, { client_id, config.cmd[1] })
        F.Notify(
          "INFO",
          "[LSP] Start " .. config.cmd[1] .. " (ID: " .. client_id .. ")"
        )
      end
      return client_id
    end
  end
  return client_id
end

---@class lsp_soft_stop_opts_t
---@field retry integer?
---@field interval integer?
---@field on_close fun(client: vim.lsp.Client)

---Soft stop LSP client with retries.
---@param client_or_id integer|vim.lsp.Client
---@param opts lsp_soft_stop_opts_t?
function LSP.stop(client_or_id, opts)
  local client = type(client_or_id) == "number"
      and vim.lsp.get_client_by_id(client_or_id)
    or client_or_id --[[@as vim.lsp.Client]]
  if not client then return end
  opts = opts or {}
  opts.retry = opts.retry or 4
  opts.interval = opts.interval or 500
  opts.on_close = opts.on_close or function() end
  if opts.retry <= 0 then
    client.stop(true)
    opts.on_close(client)
    return
  end
  client.stop()
  ---@diagnostic disable-next-line: invisible
  if client.is_stopped() then
    opts.on_close(client)
    LSP.active_clients = vim.tbl_filter(
      function(e) return e[1] ~= client.id end,
      LSP.active_clients
    )
    return
  end
  vim.defer_fn(function()
    opts.retry = opts.retry - 1
    LSP.stop(client, opts)
  end, opts.interval)
end

vim.api.nvim_create_user_command("LSPStop", function(opts)
  if opts.args then
    local client_id = tonumber(opts.args)
    if client_id ~= nil then require("util.lsp").soft_stop(client_id) end
    F.Notify("LSP", "Stopping client " .. opts.args .. ".")
  end
end, { nargs = 1, desc = "Stop LSP client with given ID." })

-- TODO:
---Restart and reattach LSP client.
---@param client_id number
function LSP.restart(client_id)
  local client = vim.lsp.get_client_by_id(client_id) --[[@as vim.lsp.Client]]
  if not client then return end
  local config = client.config
  local attached_buffers = client.attached_buffers
  F.Notify(
    "LSP",
    "@LSP.restart; Attached buffers = " .. vim.fn.string(attached_buffers)
  )
  LSP.stop(client, {
    on_close = function(c)
      for buf, _ in pairs(c.attached_buffers) do
        if vim.api.nvim_buf_is_valid(buf) then
          vim.api.nvim_buf_call(buf, function() LSP.start(config) end)
        end
      end
    end,
  })
end

vim.api.nvim_create_user_command("LSPRestart", function(opts)
  if opts.args then
    local client_id = tonumber(opts.args)
    if client_id ~= nil then
      vim.defer_fn(function() LSP.restart(client_id) end, 1)
    end
    F.Notify("LSP", "Restarting client " .. opts.args .. ".")
  end
end, { nargs = 1, desc = "Restart LSP client with given ID." })

---Show active LSP clients.
function LSP.buf_active_clients()
  local s = vim.fn.substitute
  local tbl_langs =
    s(s(vim.fn.string(LSP.active_clients), "[", "", "g"), "]", "", "g")
  if tbl_langs == "" then
    F.Notify("LSP", "No active clients.")
  else
    F.Notify("LSP", tbl_langs)
  end
end

return LSP
