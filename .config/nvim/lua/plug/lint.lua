return {
  "mfussenegger/nvim-lint",
  event = "BufWritePost",
  opts = {
    events = "BufWritePost",
    linters = {},
    linters_by_ft = {
      c = { "clangtidy" },
      cpp = { "clangtidy" },
      python = { "ruff" },
      rust = { "clippy-driver" },
      go = { "golangcilint" },
      lua = { "luacheck" },
      sh = { "shellcheck" },
      fish = { "fish" },
      bash = { "shellcheck" },
      json = { "biomejs" },
      jsonc = { "biomejs" },
      javascript = { "biomejs" },
      typescript = { "biomejs" },
    },
  },
  config = function(_, opts)
    local M = {}
    local lint = require("lint")
    lint.linters.shellcheck.args = {
      "-x",
      "-a",
      "-s bash",
      "-P $SCRIPTDIR:$PATH",
    }
    lint.linters.biomejs.args = {
      "lint",
      "--config-path=" .. vim.fn.expand("$XDG_CONFIG_PATH") .. "/biome",
    }
    for name, linter in pairs(opts.linters) do
      if type(linter) == "table" and type(lint.linters[name]) == "table" then
        lint.linters[name] =
          vim.tbl_deep_extend("force", lint.linters[name], linter)
      else
        lint.linters[name] = linter
      end
    end
    lint.linters_by_ft = opts.linters_by_ft

    function M.debounce(ms, fn)
      local timer = vim.uv.new_timer()
      if timer == nil then return nil end
      return function(...)
        local argv = { ... }
        timer:start(ms, 0, function()
          timer:stop()
          vim.schedule_wrap(fn)(unpack(argv))
        end)
      end
    end

    function M.lint()
      local names = lint._resolve_linter_by_ft(vim.bo.filetype)
      names = vim.list_extend({}, names)
      if #names == 0 then
        vim.list_extend(names, lint.linters_by_ft["_"] or {})
      end
      vim.list_extend(names, lint.linters_by_ft["*"] or {})
      local ctx = { filename = vim.api.nvim_buf_get_name(0) }
      ctx.dirname = vim.fn.fnamemodify(ctx.filename, ":h")
      names = vim.tbl_filter(function(name)
        local linter = lint.linters[name]
        if not linter then
          vim.notify(
            "Linter not found: " .. name,
            vim.log.levels.WARN,
            { title = "nvim-lint" }
          )
        end
        return linter
          and not (
            type(linter) == "table"
            and linter.condition
            and not linter.condition(ctx)
          )
      end, names)
      if #names > 0 then lint.try_lint(names) end
    end

    vim.api.nvim_create_autocmd(opts.events, {
      group = vim.api.nvim_create_augroup("nvim-lint", { clear = true }),
      callback = M.debounce(100, M.lint),
    })
  end,
}
