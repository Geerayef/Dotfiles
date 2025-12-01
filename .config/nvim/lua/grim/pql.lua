--- @class POResponse
--- @field package ErrorCode      integer
--- @field package ErrorMessageId string
--- @field package ErrorMessage   string
--- @field package ErrorSource    string
--- @field package Path           string?
--- @field package Column         integer?
--- @field package Row            integer?
--- @field package HTTPStatus     integer

--- @package
--- @param json string # HTTP response JSON
--- @return POResponse
local decode_response = function(json)
  local response =
    vim.json.decode(json, { luanil = { object = true, array = true } })
  local errors = response[1] and response[1].Return1
  if type(errors) ~= "table" or #errors ~= 1 then return {} end
  --- @type POResponse
  local err = errors[1]
  if err.ErrorCode and err.ErrorCode ~= 0 and err.ErrorMessage then
    return err
  end
  return {}
end

--- @package
--- @param bufnr integer
local execute = function(bufnr)
  if vim.bo[bufnr].filetype ~= "pqls" then
    vim.notify("Not a PQLS file", vim.log.levels.ERROR)
    return
  end
  local curl = require("plenary.curl")
  local buf_lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  local buf_contents = table.concat(buf_lines, "\n")
  local buf_filepath = vim.api.nvim_buf_get_name(bufnr)
  local ok, response =
    pcall(curl.post, "http://localhost:8989/pplx.oms/pql/execute", {
      body = vim.json.encode({
        method = "execute",
        path = buf_filepath,
        statement = buf_contents,
      }),
      headers = { content_type = "application/json" },
    })
  if not ok then
    vim.notify("Request failed: " .. tostring(response), vim.log.levels.ERROR)
    return
  end
  local body = response.body
  local error = decode_response(body)
  if error then
    if error.ErrorMessage then
      vim.notify(error.ErrorMessage, vim.log.levels.ERROR)
    end
    if error.Row and error.Path then
      local buf_target = vim.fn.bufnr(error.Path, true)
      vim.cmd.edit(vim.fn.fnameescape(error.Path))
      vim.api.nvim_win_set_cursor(0, { error.Row, error.Column })
      vim.diagnostic.set(
        vim.api.nvim_create_namespace("PQLSDiagnostics"),
        buf_target,
        {
          {
            lnum = error.Row,
            col = error.Column,
            message = error.ErrorMessage,
            severity = vim.diagnostic.severity.ERROR,
          },
        }
      )
    elseif body[1] and body[1].Return1 then
      local output = vim.json.encode(body[1].Return1)
      local buf_new = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_buf_set_lines(buf_new, 0, -1, false, vim.split(output, "\n"))
      vim.api.nvim_open_win(buf_new, true, {
        relative = "editor",
        width = 80,
        height = 20,
        col = 10,
        row = 5,
        style = "minimal",
        border = "single",
      })
      vim.bo[buf_new].filetype = "json"
    end
  end
end

return function(bufnr) execute(bufnr) end
