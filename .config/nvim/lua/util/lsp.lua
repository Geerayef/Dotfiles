LSP = {}

---@class ActiveClient
---@field id integer #LSP client ID
---@field name string #LSP client name

---@type ActiveClient[]
LSP.active_clients = {}

local has_cmplsp, cmplsp = pcall(require, "cmp_nvim_lsp")
if not has_cmplsp then F.Notify("info", "`cmp_nvim_lsp` not found.") end
local capabilities = vim.tbl_deep_extend(
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
  has_cmplsp
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

---@type lsp_client_config_t
LSP.default_config = {
  capabilities = capabilities,
  root_patterns = S.root_markers,
  single_file_support = true,
}

---Wrapper for `vim.lsp.start()`.
---Starts and attaches LSP client to the current buffer.
---@param config vim.lsp.ClientConfig -- lsp_client_config_t
---@param opts table?
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
          vim.list_extend(
            config.root_patterns or {},
            LSP.default_config.root_patterns or {}
          )
        ),
      }, LSP.default_config),
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
        F.Notify("LSP", "Start client for `" .. config.cmd[1] .. "`.")
      end
      F.Notify("LSP", "Attach to client `" .. config.cmd[1] .. "`.")
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
function LSP.soft_stop(client_or_id, opts)
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
    return
  end
  vim.defer_fn(function()
    opts.retry = opts.retry - 1
    LSP.soft_stop(client, opts)
  end, opts.interval)
end

---Restart and reattach LSP client.
---@param client_or_id integer|vim.lsp.Client
function LSP.restart(client_or_id)
  local client = type(client_or_id) == "number"
      and vim.lsp.get_client_by_id(client_or_id)
    or client_or_id --[[@as vim.lsp.Client]]
  if not client then return end
  local config = client.config
  local attached_buffers = client.attached_buffers
  LSP.soft_stop(client, {
    on_close = function()
      for buf, _ in pairs(attached_buffers) do
        if not vim.api.nvim_buf_is_valid(buf) then return end
        vim.api.nvim_buf_call(buf, function() LSP.start(config) end)
      end
    end,
  })
end

---Show active LSP clients.
---@param buf integer|nil # Buffer number
function LSP.buf_active_clients(buf)
  if buf == nil then buf = 0 end
  F.Notify("LSP", vim.fn.string(vim.g.lsp_active_clients))
end

return LSP
