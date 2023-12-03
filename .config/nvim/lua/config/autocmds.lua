local autocmd = vim.api.nvim_create_autocmd
local augroup = vim.api.nvim_create_augroup

-- ~  Highlight on yank

local highlight_group = augroup("YankHighlight", { clear = true })
autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

-- ~ -------------------------------------------------------------------------------- ~ --

-- ~  CMP

local cmp = require("cmp")
local default_cmp_src = cmp.config.sources()
-- (
  -- {
  --   { name = "nvim_lsp" },
  --   { name = "nvim_lsp_signature_help" },
  --   { name = "buffer"}
  -- },
  -- {
  --   { name = "path" },
  --   { name = "cmdline" },
  -- },
  -- { name = "luasnip" }
-- )

-- ~  Add treesitter only to buffers < 100kB
autocmd("BufReadPre", {
  callback = function(t)
    local sources = default_cmp_src
    if not F.IsBigBuff(t.buf) then
      sources[#sources+1] = { name = "treesitter", group_index = 2 }
    end
    cmp.setup.buffer {
      sources = sources
    }
  end
})

-- ~  Add Lua src only to Lua buffers
autocmd("BufReadPre", {
  callback = function()
    local sources = default_cmp_src
    if vim.b.current_syntax == "lua" then
      sources[#sources+1] = { name = "nvim_lua", group_index = 4 }
    end
    cmp.setup.buffer {
      sources = sources
    }
  end
})
