return {
  "ibhagwan/fzf-lua",
  cmd = "FzfLua",
  config = function()
    local border = require("util.objects").Border
    local actions = require("fzf-lua.actions")
    return {
      -- fzf_bin = 'sk', -- skim/fzf/fzf-tmux
      winopts = {
        -- split = "belowright new",
        -- "belowright new"  : split below
        -- "aboveleft new"   : split above
        -- "belowright vnew" : split right
        -- "aboveleft vnew   : split left
        -- Only valid when using a float window
        -- (i.e. when 'split' is not defined, default)
        height = 0.45, -- window height
        width = 0.4, -- window width
        row = 0.75, -- window row position (0=top, 1=bottom)
        col = 0.50, -- window col position (0=left, 1=right)
        border = border,
        title = "Search",
        title_pos = "left",
        fullscreen = false,
        preview = {
          -- default = "bat",
          border = "border", -- border|noborder
          wrap = "nowrap", -- wrap|nowrap
          hidden = "hidden", -- hidden|nohidden
          vertical = "down:45%", -- up|down:size
          horizontal = "right:60%", -- right|left:size
          layout = "vertical", -- horizontal|vertical|flex
          title = true,
          title_pos = "center",
          scrollbar = false,
          scrolloff = "-2",
          scrollchars = { "█", "" },
          delay = 100,
          winopts = {
            number = true,
            relativenumber = false,
            cursorline = true,
            cursorlineopt = "number",
            cursorcolumn = false,
            signcolumn = "no",
            list = false,
            foldenable = false,
            foldmethod = "manual",
          },
        },
      },
      keymap = {
        -- builtin = {
        --   -- neovim `:tmap` mappings for the fzf win
        --   ["<F1>"] = "toggle-help",
        --   ["<F2>"] = "toggle-fullscreen",
        --   -- Only valid with the 'builtin' previewer
        --   ["<F3>"] = "toggle-preview-wrap",
        --   ["<F4>"] = "toggle-preview",
        --   -- Rotate preview clockwise/counter-clockwise
        --   ["<F5>"] = "toggle-preview-ccw",
        --   ["<F6>"] = "toggle-preview-cw",
        --   ["<S-down>"] = "preview-page-down",
        --   ["<S-up>"] = "preview-page-up",
        --   ["<S-left>"] = "preview-page-reset",
        -- },
        -- fzf '--bind=' options
        fzf = {
          ["ctrl-z"] = "abort",
          ["ctrl-u"] = "unix-line-discard",
          ["ctrl-f"] = "half-page-down",
          ["ctrl-b"] = "half-page-up",
          ["ctrl-a"] = "beginning-of-line",
          ["ctrl-e"] = "end-of-line",
          ["alt-a"] = "toggle-all",
          -- Only valid with fzf previewers (bat/cat/git/etc)
          ["f3"] = "toggle-preview-wrap",
          ["f4"] = "toggle-preview",
          -- [""] = "preview-page-down",
          -- [""] = "preview-page-up",
        },
      },
      actions = {
        files = {
          -- ["default"]     = actions.file_edit,
          ["default"] = actions.file_edit_or_qf,
          ["ctrl-x"] = actions.file_split,
          ["ctrl-v"] = actions.file_vsplit,
          ["ctrl-t"] = actions.file_tabedit,
          ["alt-q"] = actions.file_sel_to_qf,
          ["alt-l"] = actions.file_sel_to_ll,
        },
        buffers = {
          ["default"] = actions.buf_edit,
          ["ctrl-x"] = actions.buf_split,
          ["ctrl-v"] = actions.buf_vsplit,
          ["ctrl-t"] = actions.buf_tabedit,
        },
      },
      fzf_opts = {
        -- options are sent as `<left>=<right>`
        -- set to `false` to remove a flag
        -- set to `true` for a no-value flag
        -- for raw args use `fzf_args` instead
        ["--ansi"] = true,
        ["--info"] = "inline",
        ["--height"] = "100%",
        ["--layout"] = "reverse",
        ["--border"] = "none",
      },
      fzf_tmux_opts = { ["-p"] = "80%,80%", ["--margin"] = "0,0" },
      -- fzf's `--color=` arguments (optional)
      -- If rhs is of type "string" rhs will be passed raw, e.g.:
      --   `["fg"] = "underline"` will be translated to `--color fg:underline`
      -- If rhs is of type "table", the following convention is used:
      --   [1] "what" field to extract from the hlgroup, i.e "fg", "bg", etc.
      --   [2] Neovim highlight group(s), can be either "string" or "table"
      --       when type is "table" the first existing highlight group is used
      --   [3+] any additional fields are passed raw to fzf's command line args
      -- Example of a "fully loaded" color option:
      --   `["fg"] = { "fg", { "NonExistentHl", "Comment" }, "underline", "bold" }`
      -- Assuming `Comment.fg=#010101` the resulting fzf command line will be:
      --   `--color fg:#010101:underline:bold`
      -- NOTE: to pass raw arguments `fzf_opts["--color"]` or `fzf_args`
      --[[ fzf_colors = {
          ["fg"]          = { "fg", "CursorLine" },
          ["bg"]          = { "bg", "Normal" },
          ["hl"]          = { "fg", "Comment" },
          ["fg+"]         = { "fg", "Normal" },
          ["bg+"]         = { "bg", "CursorLine" },
          ["hl+"]         = { "fg", "Statement" },
          ["info"]        = { "fg", "PreProc" },
          ["prompt"]      = { "fg", "Conditional" },
          ["pointer"]     = { "fg", "Exception" },
          ["marker"]      = { "fg", "Keyword" },
          ["spinner"]     = { "fg", "Label" },
          ["header"]      = { "fg", "Comment" },
          ["gutter"]      = { "bg", "Normal" },
      }, ]]
      previewers = {
        cat = {
          cmd = "cat",
          args = "-n",
        },
        bat = {
          cmd = "bat",
          args = "--color=always --style=numbers,changes",
          -- theme = 'Coldark-Dark',
        },
        head = {
          cmd = "head",
          args = nil,
        },
        git_diff = {
          -- if required, use `{file}` for argument positioning
          -- e.g. `cmd_modified = "git diff --color HEAD {file} | cut -c -30"`
          cmd_deleted = "git diff --color HEAD --",
          cmd_modified = "git diff --color HEAD",
          cmd_untracked = "git diff --color --no-index /dev/null",
          -- git-delta is automatically detected as pager, set `pager=false`
          -- to disable, can also be set under 'git.status.preview_pager'
        },
        man = {
          -- NOTE: remove the `-c` flag when using man-db
          -- replace with `man -P cat %s | col -bx` on OSX
          cmd = "man -c %s | col -bx",
        },
        builtin = {
          syntax = true, -- preview syntax highlight?
          syntax_limit_l = 0, -- syntax limit (lines), 0=nolimit
          syntax_limit_b = 1024 * 1024, -- syntax limit (bytes), 0=nolimit
          limit_b = 1024 * 1024 * 10, -- preview limit (bytes), 0=nolimit
          treesitter = { enable = true, disable = {} },
          toggle_behavior = "default",
          extensions = {
            ["png"] = { "viu", "-b" },
            ["svg"] = { "chafa", "{file}" },
            ["jpg"] = { "ueberzug" },
          },
          ueberzug_scaler = "cover",
        },
        codeaction = {
          diff_opts = { ctxlen = 3 },
        },
        codeaction_native = {
          diff_opts = { ctxlen = 3 },
          -- git-delta is automatically detected as pager, set `pager=false`
          -- to disable, can also be set under 'lsp.code_actions.preview_pager'
          -- recommended styling for delta
          --pager = [[delta --width=$COLUMNS --hunk-header-style="omit" --file-style="omit"]],
        },
      },
      -- PROVIDERS SETUP
      -- use `defaults` (table or function) if you wish to set "global-provider" defaults
      -- for example, disabling file icons globally and open the quickfix list at the top
      --   defaults = {
      --     file_icons   = false,
      --     copen        = "topleft copen",
      --   },
      files = {
        -- previewer = "bat",
        prompt = "Files❯ ",
        multiprocess = true,
        git_icons = true,
        file_icons = true,
        color_icons = true,
        path_shorten = 1,
        -- cmd = "find . -type f -printf '%P\n'",
        find_opts = [[-type f -not -path '*/\.git/*' -printf '%P\n']],
        rg_opts = [[--color=never --files --hidden --follow -g "!.git"]],
        fd_opts = [[--color=never --type f --hidden --follow --exclude .git]],
        -- cwd_header = true,
        cwd_prompt = true,
        cwd_prompt_shorten_len = 32,
        cwd_prompt_shorten_val = 1,
        toggle_ignore_flag = "--no-ignore",
        actions = {
          ["ctrl-g"] = { actions.toggle_ignore },
          --   ["default"]   = actions.file_edit,
          --   ["ctrl-y"]    = function(selected) print(selected[1]) end,
        },
      },
      git = {
        files = {
          prompt = "GitFiles❯ ",
          cmd = "git ls-files --exclude-standard",
          multiprocess = true,
          git_icons = true,
          file_icons = true,
          color_icons = true,
          -- cwd_header = true
        },
        status = {
          prompt = "GitStatus❯ ",
          cmd = "git -c color.status=false --no-optional-locks status --porcelain=v1 -u",
          multiprocess = true,
          file_icons = true,
          git_icons = true,
          color_icons = true,
          previewer = "git_diff",
          -- preview_pager = false,
          actions = {
            ["right"] = { fn = actions.git_unstage, reload = true },
            ["left"] = { fn = actions.git_stage, reload = true },
            ["ctrl-x"] = { fn = actions.git_reset, reload = true },
          },
          -- actions = {
          --   ["right"]   = false,
          --   ["left"]    = false,
          --   ["ctrl-x"]  = { fn = actions.git_reset, reload = true },
          --   ["ctrl-s"]  = { fn = actions.git_stage_unstage, reload = true },
          -- },
        },
        commits = {
          prompt = "Commits❯ ",
          cmd = [[git log --color --pretty=format:"%C(yellow)%h%Creset ]]
            .. [[%Cgreen(%><(12)%cr%><|(12))%Creset %s %C(blue)<%an>%Creset"]],
          preview = "git show --color {1}",
          -- preview_pager = false,
          actions = {
            ["default"] = actions.git_checkout,
            ["ctrl-y"] = { fn = actions.git_yank_commit, exec_silent = true },
          },
        },
        bcommits = {
          prompt = "BCommits❯ ",
          -- default preview shows a git diff vs the previous commit
          -- if you prefer to see the entire commit you can use:
          --   git show --color {1} --rotate-to={file}
          --   {1}    : commit SHA (fzf field index expression)
          --   {file} : filepath placement within the commands
          cmd = [[git log --color --pretty=format:"%C(yellow)%h%Creset ]]
            .. [[%Cgreen(%><(12)%cr%><|(12))%Creset %s %C(blue)<%an>%Creset" {file}]],
          preview = "git show --color {1} -- {file}",
          -- git-delta is automatically detected as pager, uncomment to disable
          -- preview_pager = false,
          actions = {
            ["default"] = actions.git_buf_edit,
            ["ctrl-s"] = actions.git_buf_split,
            ["ctrl-v"] = actions.git_buf_vsplit,
            ["ctrl-t"] = actions.git_buf_tabedit,
            ["ctrl-y"] = { fn = actions.git_yank_commit, exec_silent = true },
          },
        },
        branches = {
          prompt = "Branches❯ ",
          cmd = "git branch --all --color",
          preview = "git log --graph --pretty=oneline --abbrev-commit --color {1}",
          actions = {
            ["default"] = actions.git_switch,
          },
        },
        tags = {
          prompt = "Tags> ",
          cmd = [[git for-each-ref --color --sort="-taggerdate" --format ]]
            .. [["%(color:yellow)%(refname:short)%(color:reset) ]]
            .. [[%(color:green)(%(taggerdate:relative))%(color:reset)]]
            .. [[ %(subject) %(color:blue)%(taggername)%(color:reset)" refs/tags]],
          preview = [[git log --graph --color --pretty=format:"%C(yellow)%h%Creset ]]
            .. [[%Cgreen(%><(12)%cr%><|(12))%Creset %s %C(blue)<%an>%Creset" {1}]],
          actions = { ["default"] = actions.git_checkout },
        },
        stash = {
          prompt = "Stash> ",
          cmd = "git --no-pager stash list",
          preview = "git --no-pager stash show --patch --color {1}",
          actions = {
            ["default"] = actions.git_stash_apply,
            ["ctrl-x"] = { fn = actions.git_stash_drop, reload = true },
          },
        },
        icons = {
          ["M"] = { icon = "M", color = "yellow" },
          ["D"] = { icon = "D", color = "red" },
          ["A"] = { icon = "A", color = "green" },
          ["R"] = { icon = "R", color = "yellow" },
          ["C"] = { icon = "C", color = "yellow" },
          ["T"] = { icon = "T", color = "magenta" },
          ["?"] = { icon = "?", color = "magenta" },
          -- override git icons?
          -- ["M"]        = { icon = "★", color = "red" },
          -- ["D"]        = { icon = "✗", color = "red" },
          -- ["A"]        = { icon = "+", color = "green" },
        },
      },
      grep = {
        prompt = "Rg❯ ",
        input_prompt = "Grep For❯ ",
        multiprocess = true,
        git_icons = true,
        file_icons = true,
        color_icons = true,
        cmd = "rg --vimgrep",
        grep_opts = "--binary-files=without-match --line-number --recursive --color=auto --perl-regexp -e",
        rg_opts = "--column --line-number --no-heading --color=always --smart-case --max-columns=4096 -e",
        rg_glob = false,
        glob_flag = "--iglob",
        glob_separator = "%s%-%-",
        actions = {
          ["ctrl-g"] = { actions.grep_lgrep },
          -- ["ctrl-r"]   = { actions.toggle_ignore }
        },
        no_header = false,
        no_header_i = false,
      },
      args = {
        prompt = "Args❯ ",
        files_only = true,
        actions = { ["ctrl-x"] = { fn = actions.arg_del, reload = true } },
      },
      oldfiles = {
        prompt = "History❯ ",
        cwd_only = false,
        stat_file = true,
        include_current_session = false,
      },
      buffers = {
        prompt = "Buffers❯ ",
        file_icons = true,
        color_icons = true,
        sort_lastused = true,
        show_unloaded = true,
        cwd_only = false,
        cwd = nil,
        actions = {
          ["ctrl-x"] = { fn = actions.buf_del, reload = true },
        },
      },
      tabs = {
        prompt = "Tabs❯ ",
        tab_title = "Tab",
        tab_marker = "<<",
        file_icons = true,
        color_icons = true,
        actions = {
          ["default"] = actions.buf_switch,
          ["ctrl-x"] = { fn = actions.buf_del, reload = true },
        },
        fzf_opts = {
          ["--delimiter"] = "[\\):]",
          ["--with-nth"] = "2..",
        },
      },
      lines = {
        previewer = "builtin",
        prompt = "Lines❯ ",
        show_unloaded = true,
        show_unlisted = false,
        no_term_buffers = true,
        fzf_opts = {
          ["--delimiter"] = "[\\]:]",
          ["--nth"] = "2..",
          ["--tiebreak"] = "index",
          ["--tabstop"] = "1",
        },
        actions = {
          ["default"] = actions.buf_edit_or_qf,
          ["alt-q"] = actions.buf_sel_to_qf,
          ["alt-l"] = actions.buf_sel_to_ll,
        },
      },
      blines = {
        previewer = "builtin",
        prompt = "BLines❯ ",
        show_unlisted = true,
        no_term_buffers = false,
        -- start = "cursor",
        fzf_opts = {
          ["--delimiter"] = "[:]",
          ["--with-nth"] = "2..",
          ["--tiebreak"] = "index",
          ["--tabstop"] = "1",
        },
        actions = {
          ["default"] = actions.buf_edit_or_qf,
          ["alt-q"] = actions.buf_sel_to_qf,
          ["alt-l"] = actions.buf_sel_to_ll,
        },
      },
      tags = {
        prompt = "Tags❯ ",
        ctags_file = nil,
        multiprocess = true,
        file_icons = true,
        git_icons = true,
        color_icons = true,
        rg_opts = "--no-heading --color=always --smart-case",
        grep_opts = "--color=auto --perl-regexp",
        fzf_opts = { ["--info"] = "default", ["--tiebreak"] = "begin" },
        actions = {
          ["ctrl-g"] = { actions.grep_lgrep },
        },
        no_header = false,
        no_header_i = false,
      },
      btags = {
        prompt = "BTags❯ ",
        ctags_file = nil,
        ctags_autogen = true,
        multiprocess = true,
        file_icons = false,
        git_icons = false,
        rg_opts = "--color=never --no-heading",
        grep_opts = "--color=never --perl-regexp",
        fzf_opts = { ["--info"] = "default", ["--tiebreak"] = "begin" },
      },
      colorschemes = {
        prompt = "Colorschemes❯ ",
        live_preview = true, -- apply the colorscheme on preview?
        actions = { ["default"] = actions.colorscheme },
        winopts = { height = 0.55, width = 0.30 },
      },
      awesome_colorschemes = {
        prompt = "Colorschemes❯ ",
        live_preview = true,
        max_threads = 5,
        winopts = { row = 0, col = 0.99, width = 0.50 },
        fzf_opts = {
          ["--info"] = "default",
          ["--multi"] = true,
          ["--delimiter"] = "[:]",
          ["--with-nth"] = "3..",
          ["--tiebreak"] = "index",
        },
        actions = {
          ["default"] = actions.colorscheme,
          ["ctrl-g"] = { fn = actions.toggle_bg, exec_silent = true },
          ["ctrl-r"] = { fn = actions.cs_update, reload = true },
          ["ctrl-x"] = { fn = actions.cs_delete, reload = true },
        },
      },
      keymaps = {
        prompt = "Keymaps> ",
        winopts = { preview = { layout = "vertical" } },
        fzf_opts = { ["--tiebreak"] = "index" },
        ignore_patterns = { "^<SNR>", "^<Plug>" },
        actions = {
          ["default"] = actions.keymap_apply,
          ["ctrl-s"] = actions.keymap_split,
          ["ctrl-v"] = actions.keymap_vsplit,
          ["ctrl-t"] = actions.keymap_tabedit,
        },
      },
      quickfix = {
        file_icons = true,
        git_icons = true,
        only_valid = false,
      },
      quickfix_stack = {
        prompt = "Quickfix Stack> ",
        marker = ">",
      },
      lsp = {
        prompt_postfix = "❯ ",
        cwd_only = false,
        async_or_timeout = 5000,
        file_icons = true,
        git_icons = false,
        includeDeclaration = true,
        symbols = {
          async_or_timeout = true,
          symbol_style = 1,
          symbol_icons = {
            File = "󰈙",
            Module = "",
            Namespace = "󰦮",
            Package = "",
            Class = "󰆧",
            Method = "󰊕",
            Property = "",
            Field = "",
            Constructor = "",
            Enum = "",
            Interface = "",
            Function = "󰊕",
            Variable = "󰀫",
            Constant = "󰏿",
            String = "",
            Number = "󰎠",
            Boolean = "󰨙",
            Array = "󱡠",
            Object = "",
            Key = "󰌋",
            Null = "󰟢",
            EnumMember = "",
            Struct = "󰆼",
            Event = "",
            Operator = "󰆕",
            TypeParameter = "󰗴",
          },
          symbol_hl = function(s) return "@" .. s:lower() end,
          symbol_fmt = function(s, opts) return "[" .. s .. "]" end,
          child_prefix = true,
          fzf_opts = {
            ["--tiebreak"] = "begin",
            ["--info"] = "default",
          },
        },
        code_actions = {
          prompt = "Code Actions> ",
          async_or_timeout = 5000,
          previewer = "codeaction",
        },
        finder = {
          prompt = "LSP Finder> ",
          fzf_opts = { ["--info"] = "default" },
          file_icons = true,
          color_icons = true,
          git_icons = false,
          async = true,
          silent = true,
          separator = "| ",
          includeDeclaration = true,
          providers = {
            { "references", prefix = require("fzf-lua").utils.ansi_codes.blue("ref ") },
            { "definitions", prefix = require("fzf-lua").utils.ansi_codes.green("def ") },
            { "declarations", prefix = require("fzf-lua").utils.ansi_codes.magenta("decl") },
            { "typedefs", prefix = require("fzf-lua").utils.ansi_codes.red("tdef") },
            { "implementations", prefix = require("fzf-lua").utils.ansi_codes.green("impl") },
            { "incoming_calls", prefix = require("fzf-lua").utils.ansi_codes.cyan("in  ") },
            { "outgoing_calls", prefix = require("fzf-lua").utils.ansi_codes.yellow("out ") },
          },
        },
      },
      diagnostics = {
        prompt = "Diagnostics❯ ",
        cwd_only = false,
        file_icons = true,
        git_icons = false,
        diag_icons = true,
        diag_source = true,
        icon_padding = "",
        multiline = true,
        -- signs = {
        --   ["Error"] = { text = "", texthl = "DiagnosticError" },
        --   ["Warn"]  = { text = "", texthl = "DiagnosticWarn" },
        --   ["Info"]  = { text = "", texthl = "DiagnosticInfo" },
        --   ["Hint"]  = { text = "󰌵", texthl = "DiagnosticHint" },
        -- },
        --   1 or "hint"
        --   2 or "information"
        --   3 or "warning"
        --   4 or "error"
        -- severity_only:   keep any matching exact severity
        -- severity_limit:  keep any equal or more severe (lower)
        -- severity_bound:  keep any equal or less severe (higher)
      },
      complete_path = {
        cmd = nil,
        complete = { ["default"] = actions.complete },
      },
      complete_file = {
        cmd = nil,
        file_icons = true,
        color_icons = true,
        git_icons = false,
        actions = { ["default"] = actions.complete },
        winopts = { preview = { hidden = "hidden" } },
      },
      -- manpages = { previewer = "man_native" },
      -- helptags = { previewer = "help_native" },
      file_icon_padding = "",
    }
  end,
}
