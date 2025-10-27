local active_clients = {}

--- LSP on_attach function.
---@param client vim.lsp.Client
---@param bufnr integer
---@return nil
local attach = function(client, bufnr)
  Key.LSP(client, bufnr)
  if client.server_capabilities and client.server_capabilities.code_lens then
    vim.api.nvim_create_autocmd({ "BufEnter", "InsertLeave", "CursorHold" }, {
      group = vim.api.nvim_create_augroup("LSPCodeLens", { clear = true }),
      buffer = bufnr,
      callback = function() vim.lsp.codelens.refresh() end,
    })
  end
end

---@param capabilities lsp.ClientCapabilities
---@return lsp.ClientCapabilities
local capabilities_update = function(capabilities)
  local ok, blink = pcall(require, "blink.cmp")
  local capabilities_cmp = (ok and blink.get_lsp_capabilities(capabilities))
    or vim.lsp.protocol.make_client_capabilities()
  return vim.tbl_deep_extend(
    "keep",
    capabilities_cmp,
    capabilities,
    vim.lsp.config["*"].capabilities
  )
end

---Wrapper for `vim.lsp.start()`.
---Starts and attaches LSP client to the current buffer.
---@param config vim.lsp.Config -- lsp_client_config_t
---@param opts vim.lsp.start.Opts? # Options passed to the `vim.lsp.start()` function
---@return integer? client_id # ID of attached client or nil if failed
local start = function(config, opts)
  if
    config == {}
    or vim.b.bigfile
    or vim.bo.bt == "nofile"
    or type(config.cmd) ~= "table"
    or vim.fn.executable(config.cmd[1]) == 0
  then
    return nil
  end
  config.capabilities = LSP.capabilities_update(config.capabilities or {})
  local client_id = vim.lsp.start(
    vim.tbl_deep_extend("keep", config, {
      root_dir = require("util.fs").root(
        vim.api.nvim_buf_get_name(0),
        vim.list_extend(config.root_markers, S.root_markers)
      ),
    }),
    opts
  )
  if client_id ~= nil then
    if
      not vim.tbl_contains(LSP.active_clients, function(e) return e[2] == config.cmd[1] end, {
        predicate = true,
      })
    then
      table.insert(LSP.active_clients, { client_id, config.cmd[1] })
      F.notify("INFO", "[LSP] Start " .. config.cmd[1] .. " (ID: " .. client_id .. ")")
    end
    return client_id
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
local stop = function(client_or_id, opts)
  local client = type(client_or_id) == "number" and vim.lsp.get_client_by_id(client_or_id)
    or client_or_id --[[@as vim.lsp.Client]]
  if not client then return end
  opts = opts or {}
  opts.retry = opts.retry or 4
  opts.interval = opts.interval or 500
  opts.on_close = opts.on_close or function() end
  if opts.retry <= 0 then
    client:stop(true)
    opts.on_close(client)
    return
  end
  client:stop()
  if client:is_stopped() then
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

vim.api.nvim_create_user_command("LSPStop ", function(opts)
  if opts.args then
    local client_id = tonumber(opts.args)
    if client_id ~= nil then LSP.stop(client_id) end
    F.notify("LSP", "Stopping client " .. opts.args .. ".")
  end
end, { nargs = 1, desc = "Stop LSP client with given ID." })

---Restart and reattach LSP client.
---@param client_id number
local restart = function(client_id)
  local client = vim.lsp.get_client_by_id(client_id)
  if not client then return end
  local config = client.config --[[@as vim.lsp.Config]]
  local attached_buffers = client.attached_buffers
  F.notify("LSP", "@LSP.restart; Attached buffers = " .. vim.fn.string(attached_buffers))
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

vim.api.nvim_create_user_command("LSPRestart ", function(opts)
  if opts.args then
    local client_id = tonumber(opts.args)
    if client_id ~= nil then vim.defer_fn(function() LSP.restart(client_id) end, 1) end
    F.notify("LSP", "Restarting client " .. opts.args .. ".")
  end
end, { nargs = 1, desc = "Restart LSP client with given ID." })

---Show active LSP clients.
local buf_active_clients = function()
  local s = vim.fn.substitute
  local tbl_langs = s(s(vim.fn.string(LSP.active_clients), "[", "", "g"), "]", "", "g")
  if tbl_langs == "" then
    F.notify("LSP", "No active clients.")
  else
    F.notify("LSP", tbl_langs)
  end
end

LSP = {
  start = start,
  stop = stop,
  restart = restart,
  attach = attach,
  capabilities_update = capabilities_update,
  buf_active_clients = buf_active_clients,
  active_clients = active_clients,
}
return LSP
