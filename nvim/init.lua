--[[
        --using one instance of neovim--
Server:
    nvim --listen (servername) [files]

Joining to server:
    nvim --server (servername) --remote [files]

Getting servername:
    :echo v:servername
]]

--[[  colemak layout support  ]] --
-- based on https://github.com/linduxed/colemak.nvim

local colemak_mappings = {
    -- Up/down/left/right
    { modes = { "n", "v", "o", "x" }, lhs = "n",          rhs = "h",      desc = "Left (h)" },
    { modes = { "n", "v", "o", "x" }, lhs = "u",          rhs = "gk",     desc = "Visual Up (gk)" },
    { modes = { "n", "v", "o", "x" }, lhs = "e",          rhs = "gj",     desc = "Visual Down (gj)" },
    { modes = { "n", "v", "o", "x" }, lhs = "gu",         rhs = "k",      desc = "Up (k)" },
    { modes = { "n", "v", "o", "x" }, lhs = "ge",         rhs = "j",      desc = "Down (j)" },
    { modes = { "n", "v", "o", "x" }, lhs = "i",          rhs = "l",      desc = "Right (l)" },

    { modes = { "n", "o", "x" }, lhs = "N",          rhs = "H",      desc = "Left (h)" },
    { modes = { "n", "o", "x" }, lhs = "U",          rhs = "K",      desc = "Up (k)" },
    { modes = { "n", "o", "x" }, lhs = "E",          rhs = "J",      desc = "Down (j)" },
    { modes = { "n", "o", "x" }, lhs = "I",          rhs = "L",      desc = "Right (l)" },

    { modes = { "n", "o", "x" }, lhs = "<C-u>",          rhs = "<C-y>",      desc = "Up (k)" },
    { modes = { "n", "o", "x" }, lhs = "<C-e>",          rhs = "<C-e>",      desc = "Down (j)" },

    -- Word left/right
    { modes = { "n", "v", "o", "x" },      lhs = "a",          rhs = "b",      desc = "Word back" },
    { modes = { "n", "v", "o", "x" },      lhs = "A",          rhs = "B",      desc = "WORD back" },

    -- Braces & Brackets movement
    { modes = { "n", "o", "x" },      lhs = "x",          rhs = "%" },

    -- End of word left/right
    { modes = { "n", "o", "x" },      lhs = "N",          rhs = "ge",     desc = "End of word back" },
    { modes = { "n", "o", "x" },      lhs = "<C-n>",      rhs = "gE",     desc = "End of WORD back" },
    { modes = { "n", "o", "x" },      lhs = "I",          rhs = "e",      desc = "End of word forward" },
    { modes = { "n", "o", "x" },      lhs = "<C-i>",      rhs = "E",      desc = "End of WORD forward" },

    -- Move visual replace from      'r' to 'R'
    { modes = { "o", "v" },           lhs = "R",          rhs = "r",      desc = "Replace" },

    -- Copy/paste/delete/cut
    { modes = { "n", "o", "x", "v" }, lhs = "y",          rhs = "\"+y" },
    { modes = { "n", "o", "x", "v" }, lhs = "p",          rhs = "\"+p" },
    { modes = { "n", "o", "x", "v" }, lhs = "d",          rhs = "\"_d" },
    { modes = { "n", "o", "x", "v" },      lhs = "c",          rhs = "\"+c" },
    { modes = { "n", "o", "x", "v" },      lhs = "dc",         rhs = "\"_x" },
    { modes = { "n", "o", "x", "v" },      lhs = "dC",         rhs = "\"_X" },
    { modes = { "n", "o", "x", "v" },      lhs = "dd",         rhs = "\"_dd" },

    -- fixes                 
    { modes = { "n", "o", "x", "v" }, lhs = "v",          rhs = "v" },
    { modes = { "n", "o", "x" },      lhs = "ci",         rhs = "\"+ci" },
    { modes = { "n", "o", "x" },      lhs = "cc",         rhs = "\"+cc" },
    { modes = { "n", "o", "x" },      lhs = "yi",         rhs = "\"+yi" },
    { modes = { "n", "o", "x" },      lhs = "yy",         rhs = "\"+yy" },
    { modes = { "n", "o", "x" },      lhs = "vi",         rhs = "vi" },
    { modes = { "n", "o", "x", "v" }, lhs = "di",         rhs = "\"_di" },
    { modes = { "n", "o", "x", "v" }, lhs = "ca",         rhs = "\"+ca" },
    { modes = { "n", "o", "x", "v" }, lhs = "ya",         rhs = "\"+ya" },
    { modes = { "n", "o", "x" },      lhs = "va",         rhs = "va" },
    { modes = { "n", "o", "x", "v" }, lhs = "da",         rhs = "\"_da" },
    { modes = { "n", "o", "x" },      lhs = "gd",         rhs = "gd" },
    { modes = { "o" },                lhs = "i",          rhs = "i" },
    { modes = { "o" },                lhs = "a",          rhs = "a" },

    -- Undo/redo
    { modes = { "n" },                lhs = "l",          rhs = "u" },
    { modes = { "n" },                lhs = "gz",         rhs = "U" },
    { modes = { "n" },                lhs = "L",          rhs = "<C-r>" },

    -- insert/append (T)
    { modes = { "n" },                lhs = "j",          rhs = "i" },
    { modes = { "n" },                lhs = "J",          rhs = "I" },
    { modes = { "n" },                lhs = "b",          rhs = "a" },
    { modes = { "n" },                lhs = "B",          rhs = "A" },

    -- Insert in Visual mode
    { modes = { "v" },                lhs = "J",          rhs = "I" },
    { modes = { "v" },                lhs = "B",          rhs = "A" },

    -- Search
    { modes = { "n", "o", "x", "v" },      lhs = "k",          rhs = "n" },
    { modes = { "n", "o", "x", "v" },      lhs = "K",          rhs = "N" },

    -- Window navigation
    { modes = { "n" },                lhs = "<C-w>n",     rhs = "<C-w>h" },
    { modes = { "n" },                lhs = "<C-w>u",     rhs = "<C-w>k" },
    { modes = { "n" },                lhs = "<C-w>e",     rhs = "<C-w>j" },
    { modes = { "n" },                lhs = "<C-w>i",     rhs = "<C-w>l" },
    { modes = { "n" },                lhs = "<C-w>N",     rhs = "<C-w>H" },
    { modes = { "n" },                lhs = "<C-w>U",     rhs = "<C-w>K" },
    { modes = { "n" },                lhs = "<C-w>E",     rhs = "<C-w>J" },
    { modes = { "n" },                lhs = "<C-w>I",     rhs = "<C-w>L" },
    { modes = { "t", "i" },           lhs = "<C-w>t", rhs = "<C-\\><C-n>" },

    --other
    { modes = { "n" },                lhs = "<C-z>",       rhs = "" }, 
    { modes = { "n" },                lhs = "ZQ",          rhs = "" }, 
    { modes = { "n" },                lhs = "<M-d>",       rhs = "<cmd>lua vim.diagnostic.open_float()<CR>" },
    { modes = { "n" },                lhs = ";",           rhs = "q:" },
    { modes = { "n", "v", "o", "x" }, lhs = "g|",          rhs = "g~" },
    { modes = { "n" },                lhs = "grn",         rhs = "grn" },
    { modes = { "n" },                lhs = "grr",         rhs = "grr" },
    { modes = { "n" },                lhs = "gri",         rhs = "gri" },
    { modes = { "n" },                lhs = "g0",          rhs = "g0" },
    { modes = { "v" },                lhs = ">",           rhs = ">gv" },
    { modes = { "v" },                lhs = "<",          rhs = "<gv" },
    { modes = { "n" },                lhs = "<C-h>h",          rhs = "<cmd>lua require('haskell-tools').hoogle.hoogle_signature()<CR>" },
    { modes = { "n" },                lhs = "<C-h>rp",          rhs = "<cmd>lua require('haskell-tools').repl.toggle()<CR>" },
    { modes = { "n" },                lhs = "<C-h>rb",          rhs = "<cmd>lua require('haskell-tools').repl.toggle(vim.api.nvim_buf_get_name(0))<CR>" },
}

function colemak_apply()
    for _, mapping in pairs(colemak_mappings) do
        vim.keymap.set(
            mapping.modes,
            mapping.lhs,
            mapping.rhs,
            { desc = mapping.desc, noremap = true }
        )
    end
end

function colemak_unapply()
    for _, mapping in pairs(colemak_mappings) do
        vim.keymap.del(mapping.modes, mapping.lhs)
    end
end

function colemak_setup(_)
    colemak_apply()

    vim.api.nvim_create_user_command(
        "ColemakEnable",
        colemak_apply,
        { desc = "Applies Colemak mappings" }
    )
    vim.api.nvim_create_user_command(
        "ColemakDisable",
        colemak_unapply,
        { desc = "Removes Colemak mappings" }
    )
end

colemak_setup()

-- pckr

local function bootstrap_pckr()
  local pckr_path = vim.fn.stdpath("data") .. "/pckr/pckr.nvim"

  if not (vim.uv or vim.loop).fs_stat(pckr_path) then
    vim.fn.system({
      'git',
      'clone',
      "--filter=blob:none",
      'https://github.com/lewis6991/pckr.nvim',
      pckr_path
    })
  end

  vim.opt.rtp:prepend(pckr_path)
end

bootstrap_pckr()

local cmd = require('pckr.loader.cmd')
local event = require('pckr.loader.event')

require('pckr').add({

    --lsp related stuff--
    { 'neoclide/coc.nvim', branch = 'release' },
    { 'neovim/nvim-lspconfig' },
    {
        'mrcjkb/haskell-tools.nvim',
        version = '^6', -- Recommended
    },


    --syntax highlight--
    'bfrg/vim-c-cpp-modern',
    'gleam-lang/gleam.vim',
    {
        'nvim-treesitter/nvim-treesitter',
        branch = 'master',
        run = function()
            local ts_update = require('nvim-treesitter.install').update({ with_sync = true })
            ts_update()
        end,
        config = function()
            require('nvim-treesitter.configs').setup {
                -- Add your treesitter configuration options here
                ensure_installed = { "gleam", "ada", "haskell" },
                highlight = {
                    enable = true,
                },
                indent = {
                    enable = true,
                },
            }
        end,
    },

    --statusline and buffer line--
    'nvim-lualine/lualine.nvim',

    {
        'xiyaowong/transparent.nvim',
        config = function()
            require("transparent").setup({
              -- table: default groups
              groups = {
              --'Normal', 'NormalNC', 'Comment', 'Constant', 'Special', 'Identifier',
              --'Statement', 'PreProc', 'Type', 'Underlined', 'Todo', 'String', 'Function',
              --'Conditional', 'Repeat', 'Operator', 'Structure', 'LineNr', 'NonText',
              --'SignColumn', 'CursorLine', 'CursorLineNr', 'StatusLine', 'StatusLineNC',
              --'EndOfBuffer',
              }, -- uncomment to get transparent
              -- table: additional groups that should be cleared
              extra_groups = {},
              -- table: groups you don't want to clear
              exclude_groups = {},
              -- function: code to be executed after highlight groups are cleared
              -- Also the user event "TransparentClear" will be triggered
              on_clear = function() end,
            })
        end
    },

    --fun--
    --shows keys lol
    {
        "nvzone/showkeys",
        requires = "nvzone/volt",
        cond = cmd("ShowkeysToggle"),
    },

    { --compile mode from emacs in neovim, but better
        'pohlrabi404/compile.nvim',
        config = function()
            require("compile").setup({
                -- Give your terminal a custom name.
                term_win_name = "compilation",
                ---@type vim.api.keyset.win_config
                term_win_opts = {
                    -- The split direction for the terminal window. "below" places it at the bottom.
                    split = "below",
                    -- The height of the terminal window as a percentage (0.4 = 40%).
                    -- Any number >= 1 will use that amount of lines as height
                    height = 0.4,
                    -- width = 0.8, -- the same applied for width
                    -- Or you can make it float, adding borders, etc. check :h win_config
                },

                ---@type vim.api.keyset.win_config
                normal_win_opts = {
                    -- The split direction for the normal window. "above" places it at the top.
                    split = "above",
                    -- similar to term_win_opts
                    height = 0.6,
                },

                ---@type boolean
                -- Set this to `true` if you want to jump into the terminal when you run compile command
                enter = true,

                highlight_under_cursor = {
                    -- Enable or disable highlighting the error under your cursor. It’s a great visual cue!
                    enabled = true,
                    -- The timeout in milliseconds for the highlight to appear in the terminal.
                    timeout_term = 500,
                    -- The timeout in milliseconds for the highlight in a normal buffer.
                    timeout_normal = 200,
                },

                patterns = {
                    -- A table of patterns to match compiler output. This is how the plugin finds
                    -- files, lines, and columns for errors. The "123" and "12" refer to the
                    -- capture groups in the regex.
                    -- 1 stands for filename
                    -- 2 stands for row number
                    -- 3 stands for col number (can be omitted if the language doesn't support)
                    -- For example: col:filename:row will be "312" instead
                    -- rust = { "(%S+):(%d+):(%d+)", "123" },
                    grep = { "(%S+):(%d+):", "12" },
                    dlang = { "(%S+)%((%d+)%):", "12" },
                    Makefile = { "%[(%S+):(%d+):.+%]", "12" },
                    -- I will also add more regex for different error types soon
                },

                colors = {
                    -- Customize the highlight colors for different parts of the error message.
                    -- These correspond to Neovim highlight groups.
                    file = "WarningMsg",
                    row = "CursorLineNr",
                    col = "CursorLineNr",
                },

                keys = {
                    -- Here's where you define all the handy keybindings!
                    global = {
                        -- Normal mode keybindings, you can group modes by writing them next to each other
                        -- eg: ["nvi"] for normal, select and insert mode keybinding
                        ["n"] = {
                            -- start compile/recompile, will also open the terminal
                            ["<C-k>s"] = "require('compile').compile('')",
                            ["<C-k>k"] = "require('compile').next_error()",
                            ["<C-k>K"] = "require('compile').prev_error()",
                            ["<C-k>K"] = "require('compile').next_error()",
                            ["<C-k><C-k>"] = "require('compile').next_error()",
                            ["<C-k><C-K>"] = "require('compile').prev_error()",
                            ["<C-k>l"] = "require('compile').last_error()",
                            ["<C-k>f"] = "require('compile').first_error()",
                        },
                    },
                    term = {
                        -- Keybindings specific to the terminal buffer.
                        -- Global keybinding for terminal will work everywhere but will be removed
                        -- when you close the terminal buffer
                        global = {
                            ["n"] = {
                                -- clears the terminal
                                ["<C-k>c"] = "require('compile').clear()",
                                -- quits the terminal buffer.
                                ["<C-k>q"] = "require('compile').destroy()",
                            },
                        },
                        -- This one will only work INSIDE the terminal buffer
                        buffer = {
                            ["n"] = {
                                ["r"] = "require('compile').clear()",
                                ["c"] = "require('compile').compile()",
                                -- quit the terminal.
                                ["q"] = "require('compile').destroy()",
                                ["k"] = "require('compile').next_error()",
                                ["K"] = "require('compile').prev_error()",
                                ["0"] = "require('compile').first_error()",
                                ["$"] = "require('compile').last_error()",
                                -- Jump to the nearest error under or before your cursor
                                ["<Cr>"] = "require('compile').nearest_error()",
                            },
                            -- Tricks to clear warning/error list
                            ["t"] = {
                                -- Press `<CR>` in terminal mode to send a command and clear highlights.
                                ["<CR>"] = "require('compile').clear_hl()",
                                -- This sends the command to the terminal without clearing the error list!
                                ["<C-j>"] = "require('compile.term').send_cmd('')",
                            },
                        },
                    },
                },
            })
        end,
    },

    {
        "stevearc/oil.nvim",
        config = function()
                require("oil").setup({
                    columns = {
                        "permissions",
                        "size",
                        "mtime"
                    },
                keymaps = {
                   ["g?"] = { "actions.show_help", mode = "n" },
                   ["<CR>"] = "actions.select",
                   ["<C-s>"] = { "actions.select", opts = { vertical = true } },
                   ["<C-h>"] = { "actions.select", opts = { horizontal = true } },
                   ["<C-t>"] = { "actions.select", opts = { tab = true } },
                   ["<C-p>"] = "actions.preview",
                   ["<C-c>"] = { "actions.close", mode = "n" },
                   ["<C-l>"] = "actions.refresh",
                   ["-"] = { "actions.open_cwd", mode = "n" },
                   ["_"] = { "actions.cd", mode = "n" },
                   ["gs"] = { "actions.change_sort", mode = "n" },
                   ["gx"] = "actions.open_external",
                   ["g."] = { "actions.toggle_hidden", mode = "n" },
                   ["g\\"] = { "actions.toggle_trash", mode = "n" },
                },
                use_default_keymaps = true,
                view_options = {
                    show_hidden = true,
                }
            })
        end,
    },

    -- additional motions
    "tommcdo/vim-exchange",
    {
        "kylechui/nvim-surround",
        tag = "*", -- Use for stability; omit to use `main` branch for the latest features
        config = function()
            require("nvim-surround").setup({
                -- Configuration here, or leave empty to use defaults
            })
        end
    },

    {
        "windwp/nvim-autopairs",
        cond = event("InsertEnter"),
        config = function()
            require("nvim-autopairs").setup {}
        end
    },
    "rktjmp/lush.nvim",
    'sphamba/smear-cursor.nvim', -- smooth cursor
    'markonm/traces.vim',        -- highlights patterns in command mode
    {
        'madskjeldgaard/cppman.nvim',
        requires = {
            { 'MunifTanjim/nui.nvim' }
        },
        config = function()
            local cppman = require"cppman"
            cppman.setup()

            -- Make a keymap to open the word under cursor in CPPman
            vim.keymap.set("n", "cppu", function()
                cppman.open_cppman_for(vim.fn.expand("<cword>"))
            end)

            -- Open search box
            vim.keymap.set("n", "cpps", function()
                cppman.input()
            end)

        end
    },

    { -- nvim as kitty scrollback buffer
        'mikesmithgh/kitty-scrollback.nvim',
        disable = false,
        opt = true,
        cmd = { 'KittyScrollbackGenerateKittens', 'KittyScrollbackCheckHealth', 'KittyScrollbackGenerateCommandLineEditing' },
        event = { 'User KittyScrollbackLaunch' },
        -- tag = '*', -- latest stable version, may have breaking changes if major version changed
        -- tag = 'v6.0.0', -- pin specific tag
        config = function()
            require('kitty-scrollback').setup({
                { 
                    visual_selection_highlight_mode = 'reverse',
                    status_window = {
                        enabled = true,
                        style_simple = true,
                    },
                },
        })
        end,
    },

    'lambdalisue/vim-suda', -- saving files with sudo
    -- [[ Themes ]] -- 
    'iddixx/alabaster-bold.nvim',           -- my fork of p00f/alabaster.nvim
    'estheruary/nvim-colorscheme-lavender', -- use with ai bg
    'fenetikm/falcon',                      -- use with nullscapes bg
    'wincent/base16-nvim',
    'ntk148v/komau.vim',
    'andreypopp/vim-colors-plain',
    {
        'wtfox/jellybeans.nvim',
        config = function() 
            require('jellybeans').setup({
                    transparent = false,
                    italics = false,
                    bold = true,
                    flat_ui = true, -- toggles "flat UI" for pickers
                    -- background = {
                    --     dark = "jellybeans", -- default dark palette
                    --     light = "jellybeans_light", -- default light palette
                    -- },
                    plugins = {
                        all = false,
                        auto = true, -- will read lazy.nvim and apply the colors for plugins that are installed
                    },
                    -- on_highlights = function(highlights, colors)
                    -- end,
                    -- on_colors = function(colors)
                    -- end,
                })
        end,
    },
    {
        'iddixx/gruber-darker.nvim',
        config = function()
            require("gruber-darker").setup({
                bold = true,
                invert = {
                    signs = false,
                    tabline = false,
                    visual = false,
                },
                italic = {
                    strings = false,
                    comments = false,
                    operators = false,
                    folds = false,
                },
                undercurl = true,
                underline = false,
            })
        end,
    },
})
-- gui settings
if vim.g.neovide then
    vim.o.guifont = "Iosevka Fixed SS14:h18"
    vim.g.neovide_hide_mouse_when_typing = true
    vim.g.neovide_cursor_animation_length = 0.08

    vim.g.neovide_scale_factor = 1.0
    local scale_percentage = 0.1

    local function change_scale_factor(delta)
      vim.g.neovide_scale_factor = vim.g.neovide_scale_factor + delta
    end
    vim.keymap.set("n", "<C-=>", function()
      change_scale_factor(scale_percentage)
    end)
    vim.keymap.set("n", "<C-->", function()
      change_scale_factor(-scale_percentage)
    end)
end


--[[ Plugins Setup ]]

--require("smear_cursor").toggle()

--[[ LSP Setup ]]

vim.lsp.enable('gleam')
vim.lsp.enable('omnisharp')

--[[ Editor ]]
vim.g.loaded_netrw       = 1
vim.g.loaded_netrwPlugin = 1
vim.cmd("syntax on")
vim.cmd("filetype on")
vim.cmd("filetype indent on")
vim.cmd("set t_Co=256")
vim.cmd("set showcmd!")

-- statusline and command mode things
vim.opt.cmdheight=0
vim.opt.laststatus=0
vim.opt.virtualedit='all'

-- vim.cmd("au VimLeave * set guicursor=a:hor100") -- for underline
vim.cmd("au VimLeave * set guicursor=a:ver100") -- for beam cursor
-- vim.cmd("au VimLeave * set guicursor=a:block" ) -- for block(you maybe wont need it)

-- command aliases
vim.cmd("command B buffers")
vim.cmd("command C below Compile")
vim.cmd("command W wincmd w")

vim.api.nvim_create_user_command("O", 
    function(opts)
        require("oil").open(opts.fargs[1])
    end, {
        nargs = "?", 
        desc = "Open directory with Oil",
    })

vim.cmd("set undofile")
vim.cmd("set undodir=~/.nvim/undo")

-- line numbers
vim.opt.number         = true
vim.opt.relativenumber = true
vim.opt.cursorline     = true
-- clipboard
vim.opt.clipboard      = 'unnamedplus'

--window auto extend
vim.opt.wrap           = true

-- encoding
vim.opt.encoding       = 'utf-8'

-- status bar
vim.opt.laststatus     = 2
-- dont show -- INSERT -- -- VISUAL -- and etc
vim.cmd("set noshowmode")

-- english lang
if vim.uv.os_uname().sysname == "Windows_NT" then
    vim.api.nvim_exec('language en_US', true)
end

-- tab options
vim.opt.expandtab   = true
vim.opt.tabstop     = 4
vim.opt.shiftwidth  = 4
vim.opt.autoindent  = true
vim.opt.smarttab    = true
vim.opt.softtabstop = 4
vim.opt.cindent     = true

---------   theme  ---------

-- lualine


function get_ramrem_lualine_theme(is_ram_theme)
    local colors = {
        black     = '#000000',
        white     = '#eeeeee',
        darkwhite = '#d4d4d4',
        red       = '#ffa0a0',
        green     = '#88cf88',
        blue      = '#78a9ff',
        magenta   = '#feacd0',
        cyan      = '#a0bfdf',
        gray      = '#2f2f2f',
        darkgray  = '#202020',
        lightgray = '#434343'
    }
    if is_ram_theme then
        colors.blue    = "#fd7dc6" -- e6466a replaces blue color with pink
        colors.magenta = "#655689" -- 7f6cac makes magenta darker
    end
    local theme = {
        normal = {
            a = { bg = colors.white, fg = colors.blue, gui = 'bold' },
            b = { bg = colors.darkwhite, fg = colors.blue },
            c = { bg = nil, fg = colors.white },
        },
        insert = {
            a = { bg = colors.gray, fg = colors.darkwhite, gui = 'bold' },
            b = { bg = colors.darkwhite, fg = colors.blue },
            c = { bg = nil, fg = colors.white },
        },
        visual = {
            a = { bg = colors.gray, fg = colors.magenta, gui = 'bold' },
            b = { bg = colors.white, fg = colors.blue },
            c = { bg = nil, fg = colors.white },
        },
        replace = {
            a = { bg = colors.gray, fg = colors.magenta, gui = 'bold' },
            b = { bg = colors.darkwhite, fg = colors.red },
            c = { bg = nil, fg = colors.white },
        },
        command = {
            a = { bg = colors.blue, fg = colors.magenta, gui = 'bold' },
            b = { bg = colors.darkwhite, fg = colors.gray },
            c = { bg = nil, fg = colors.white },
        },
        inactive = {
            a = { bg = colors.gray, fg = colors.magenta, gui = 'bold' },
            b = { bg = colors.darkwhite, fg = colors.gray, gui = 'bold' },
            c = { bg = nil, fg = colors.lightgray },
        },
    }
    return theme
end

local nf_colors = {
    color3 = '#2c3043',
    color6 = '#a1aab8',
    color7 = '#9b82ff',
    color8 = '#ae81ff',
    color0 = '#092236',
    color1 = '#ff5874',
    color2 = '#c3ccdc',
}

local nf_theme = {
    replace = {
        a = { fg = nf_colors.color0, bg = nf_colors.color1, gui = 'bold' },
        b = { fg = nf_colors.color2, bg = nf_colors.color3 },
        c = { fg = nf_colors.color6, bg = nil },
    },
    inactive = {
        a = { fg = nf_colors.color6, bg = nf_colors.color3, gui = 'bold' },
        b = { fg = nf_colors.color6, bg = nf_colors.color3 },
        c = { fg = nf_colors.color6, bg = nil },
    },
    normal = {
        a = { fg = nf_colors.color0, bg = nf_colors.color7, gui = 'bold' },
        b = { fg = nf_colors.color2, bg = nf_colors.color3 },
        c = { fg = nf_colors.color2, bg = nil },
    },
    visual = {
        a = { fg = nf_colors.color0, bg = nf_colors.color8, gui = 'bold' },
        b = { fg = nf_colors.color2, bg = nf_colors.color3 },
        c = { fg = nf_colors.color6, bg = nil },
    },
    insert = {
        a = { fg = nf_colors.color0, bg = nf_colors.color2, gui = 'bold' },
        b = { fg = nf_colors.color2, bg = nf_colors.color3 },
        c = { fg = nf_colors.color6, bg = nil },
    },
}

function generate_random_text(text)
    local text_table = {"the", "be", "of", "and", "a", "to", "in", "he", "have", "it", "that", "for", "they", "I", "with", "as", "not", "on", "she", "at", "by", "this", "we", "you", "do", "but", "from", "or", "which", "one", "would", "all", "will", "there", "say", "who", "make", "when", "can", "more", "if", "no", "man", "out", "other", "so", "what", "time", "up", "go", "about", "than", "into", "could", "state", "only", "new", "year", "some", "take", "come", "these", "know", "see", "use", "get", "like", "then", "first", "any", "work", "now", "may", "such", "give", "over", "think", "most", "even", "find", "day", "also", "after", "way", "many", "must", "look", "before", "great", "back", "through", "long", "where", "much", "should", "well", "people", "down", "own", "just", "because", "good", "each", "those", "feel", "seem", "how", "high", "too", "place", "little", "world", "very", "still", "nation", "hand", "old", "life", "tell", "write", "become", "here", "show", "house", "both", "between", "need", "mean", "call", "develop", "under", "last", "right", "move", "thing", "general", "school", "never", "same", "another", "begin", "while", "number", "part", "turn", "real", "leave", "might", "want", "point", "form", "off", "child", "few", "small", "since", "against", "ask", "late", "home", "large", "person", "end", "open", "public", "follow", "during", "present", "without", "again", "hold", "govern", "around", "head", "word", "program", "problem", "however", "lead", "system", "set", "order", "eye", "plan", "run", "keep", "face", "fact", "group", "play", "stand", "early", "course", "change", "help", "line"}

    local result = ""
    for i=1,math.random(1, 3) do
        result = result .. text_table[math.random(100)] .. " "
    end
    result = string.sub(result, 1, (string.len(result) - 1)) -- removing last space
    return result
end

require('lualine').setup {
    options = {
        icons_enabled = false,
        -- theme = get_ramrem_lualine_theme(true),
        theme = "auto",
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        disabled_filetypes = {
            statusline = {},
            winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        always_show_tabline = true,
        globalstatus = false,
        refresh = {
            statusline = 100,
            tabline = 100,
            winbar = 100,
        }
    },
    sections = {
        lualine_a = { 'location', 'selectioncount' },
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = { 'encoding' },
        lualine_z = { 'filename' }
    },
    inactive_sections = {
      --lualine_a = {},
      --lualine_b = {},
      --lualine_c = { 'filename' },
      --lualine_x = { 'location' },
      --lualine_y = {},
      --lualine_z = {}
    },
    tabline = {
    },
    winbar = {},
    inactive_winbar = {},
    extensions = {}
}

vim.opt.termguicolors = true
-- vim.opt.background = "light"
vim.opt.background = "dark"

vim.g.komau_italic = false
vim.g["suda#prompt"] = "Porn folder encryption key: "

--vim.cmd.colorscheme("lavender") -- use with ai bg
--vim.cmd.colorscheme("base16-stella") -- use with purple anime girl bg
--vim.cmd.colorscheme("falcon") -- use with nullspaces
--vim.cmd.colorscheme("yuyuko") -- use with rem bg
--vim.cmd.colorscheme("base16-everforest-dark-hard") -- use with komari bg
--vim.cmd.colorscheme("komau") -- cool monochrome theme
--vim.cmd.colorscheme("plain") -- other cool monochrome theme
-- vim.cmd.colorscheme("alabaster") -- i like this theme
vim.cmd.colorscheme("jellybeans-mono") -- dark theme


-- cpp higlight
vim.g.cpp_function_highlight = false
vim.g.cpp_attributes_highlight = true
vim.g.cpp_member_highlight = true
vim.g.cpp_type_name_highlight = false
vim.g.cpp_operator_highlight = true
vim.g.cpp_simple_highlight = false



--[[ coc.nvim ]]
vim.g.coc_global_extensions = { 'coc-clangd', 'coc-discord-rpc',
    'coc-cmake',  'coc-highlight', 'coc-tabnine' }
-- Some servers have issues with backup files, see #649
vim.opt.backup = false
vim.opt.writebackup = false

-- Having longer updatetime (default is 4000 ms = 4s) leads to noticeable
-- delays and poor user experience
vim.opt.updatetime = 300

-- Always show the signcolumn, otherwise it would shift the text each time
-- diagnostics appeared/became resolved
vim.opt.signcolumn = "yes"

local keyset = vim.keymap.set
-- Autocomplete
function _G.check_back_space()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

-- Use Tab for trigger completion with characters ahead and navigate
-- NOTE: There's always a completion item selected by default, you may want to enable
-- no select by setting `"suggest.noselect": true` in your configuration file
-- NOTE: Use command ':verbose imap <tab>' to make sure Tab is not mapped by
-- other plugins before putting this into your config
local opts = { silent = true, noremap = true, expr = true, replace_keycodes = false }
keyset("i", "<TAB>", 'coc#pum#visible() ? coc#pum#next(1) : v:lua.check_back_space() ? "<TAB>" : coc#refresh()', opts)
keyset("i", "<S-TAB>", [[coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"]], opts)

-- Make <CR> to accept selected completion item or notify coc.nvim to format
-- <C-g>u breaks current undo, please make your own choice
keyset("i", "<shift>", [[coc#pum#visible() ? coc#pum#confirm() : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"]], opts)

-- Use <c-j> to trigger snippets
keyset("i", "<c-j>", "<Plug>(coc-snippets-expand-jump)")
-- Use <c-space> to trigger completion
keyset("i", "<c-space>", "coc#refresh()", { silent = true, expr = true })

-- Use `[g` and `]g` to navigate diagnostics
-- Use `:CocDiagnostics` to get all diagnostics of current buffer in location list
keyset("n", "[g", "<Plug>(coc-diagnostic-prev)", { silent = true })
keyset("n", "]g", "<Plug>(coc-diagnostic-next)", { silent = true })

-- GoTo code navigation
keyset("n", "gd", "<Plug>(coc-definition)", { silent = true })
keyset("n", "gy", "<Plug>(coc-type-definition)", { silent = true })
keyset("n", "gi", "<Plug>(coc-implementation)", { silent = true })
keyset("n", "gr", "<Plug>(coc-references)", { silent = true })


-- Use K to show documentation in preview window
function _G.show_docs()
    local cw = vim.fn.expand('<cword>')
    if vim.fn.index({ 'vim', 'help' }, vim.bo.filetype) >= 0 then
        vim.api.nvim_command('h ' .. cw)
    elseif vim.api.nvim_eval('coc#rpc#ready()') then
        vim.fn.CocActionAsync('doHover')
    else
        vim.api.nvim_command('!' .. vim.o.keywordprg .. ' ' .. cw)
    end
end

--keyset("n", "K", '<CMD>lua _G.show_docs()<CR>', { silent = true })


-- Highlight the symbol and its references on a CursorHold event(cursor is idle)
vim.api.nvim_create_augroup("CocGroup", {})
vim.api.nvim_create_autocmd("CursorHold", {
    group = "CocGroup",
    command = "silent call CocActionAsync('highlight')",
    desc = "Highlight symbol under cursor on CursorHold"
})


-- Symbol renaming
keyset("n", "<leader>rn", "<Plug>(coc-rename)", { silent = true })


-- Formatting selected code
keyset("x", "<leader>f", "<Plug>(coc-format-selected)", { silent = true })
keyset("n", "<leader>f", "<Plug>(coc-format-selected)", { silent = true })


-- Setup formatexpr specified filetype(s)
vim.api.nvim_create_autocmd("FileType", {
    group = "CocGroup",
    pattern = "typescript,json",
    command = "setl formatexpr=CocAction('formatSelected')",
    desc = "Setup formatexpr specified filetype(s)."
})

-- Update signature help on jump placeholder
vim.api.nvim_create_autocmd("User", {
    group = "CocGroup",
    pattern = "CocJumpPlaceholder",
    command = "call CocActionAsync('showSignatureHelp')",
    desc = "Update signature help on jump placeholder"
})

-- Apply codeAction to the selected region
-- Example: `<leader>aap` for current paragraph
local opts = { silent = true, nowait = true }
keyset("x", "<leader>a", "<Plug>(coc-codeaction-selected)", opts)
keyset("n", "<leader>a", "<Plug>(coc-codeaction-selected)", opts)

-- Remap keys for apply code actions at the cursor position.
keyset("n", "<leader>ac", "<Plug>(coc-codeaction-cursor)", opts)
-- Remap keys for apply source code actions for current file.
keyset("n", "<leader>as", "<Plug>(coc-codeaction-source)", opts)
-- Apply the most preferred quickfix action on the current line.
keyset("n", "<leader>qf", "<Plug>(coc-fix-current)", opts)

-- Remap keys for apply refactor code actions.
keyset("n", "<leader>re", "<Plug>(coc-codeaction-refactor)", { silent = true })
keyset("x", "<leader>r", "<Plug>(coc-codeaction-refactor-selected)", { silent = true })
keyset("n", "<leader>r", "<Plug>(coc-codeaction-refactor-selected)", { silent = true })

-- Run the Code Lens actions on the current line
keyset("n", "<leader>cl", "<Plug>(coc-codelens-action)", opts)


-- Map function and class text objects
-- NOTE: Requires 'textDocument.documentSymbol' support from the language server
-- keyset("x", "if", "<Plug>(coc-funcobj-i)", opts)
-- keyset("o", "if", "<Plug>(coc-funcobj-i)", opts)
-- keyset("x", "af", "<Plug>(coc-funcobj-a)", opts)
-- keyset("o", "af", "<Plug>(coc-funcobj-a)", opts)
-- keyset("x", "ic", "<Plug>(coc-classobj-i)", opts)
-- keyset("o", "ic", "<Plug>(coc-classobj-i)", opts)
-- keyset("x", "ac", "<Plug>(coc-classobj-a)", opts)
-- keyset("o", "ac", "<Plug>(coc-classobj-a)", opts)
--

-- Remap  and <C-b> to scroll float windows/popups
---@diagnostic disable-next-line: redefined-local
local opts = { silent = true, nowait = true, expr = true }
keyset("n", "<C-f>", 'coc#float#has_scroll() ? coc#float#scroll(1) : "<C-f>"', opts)
keyset("n", "<C-b>", 'coc#float#has_scroll() ? coc#float#scroll(0) : "<C-b>"', opts)
keyset("i", "<C-f>",
    'coc#float#has_scroll() ? "<c-r>=coc#float#scroll(1)<cr>" : "<Right>"', opts)
keyset("i", "<C-b>",
    'coc#float#has_scroll() ? "<c-r>=coc#float#scroll(0)<cr>" : "<Left>"', opts)
keyset("v", "<C-f>", 'coc#float#has_scroll() ? coc#float#scroll(1) : "<C-f>"', opts)
keyset("v", "<C-b>", 'coc#float#has_scroll() ? coc#float#scroll(0) : "<C-b>"', opts)


-- Use CTRL-S for selections ranges
-- Requires 'textDocument/selectionRange' support of language server
keyset("n", "<C-s>", "<Plug>(coc-range-select)", { silent = true })
keyset("x", "<C-s>", "<Plug>(coc-range-select)", { silent = true })


-- Add `:Format` command to format current buffer
vim.api.nvim_create_user_command("Format", "call CocAction('format')", {})

-- " Add `:Fold` command to fold current buffer
vim.api.nvim_create_user_command("Fold", "call CocAction('fold', <f-args>)", { nargs = '?' })

-- Add `:OR` command for organize imports of the current buffer
vim.api.nvim_create_user_command("OR", "call CocActionAsync('runCommand', 'editor.action.organizeImport')", {})

-- Add (Neo)Vim's native statusline support
-- NOTE: Please see `:h coc-status` for integrations with external plugins that
-- provide custom statusline: lightline.vim, vim-airline
vim.opt.statusline:prepend("%{coc#status()}%{get(b:,'coc_current_function','')}")

-- Mappings for CoCList
-- code actions and coc stuff
---@diagnostic disable-next-line: redefined-local
local opts = { silent = true, nowait = true }
-- Show all diagnostics
keyset("n", "<space>a", ":<C-u>CocList diagnostics<cr>", opts)
-- Manage extensions
keyset("n", "<space>e", ":<C-u>CocList extensions<cr>", opts)
-- Show commands
keyset("n", "<space>c", ":<C-u>CocList commands<cr>", opts)
-- Find symbol of current document
keyset("n", "<space>o", ":<C-u>CocList outline<cr>", opts)
-- Search workspace symbols
keyset("n", "<space>s", ":<C-u>CocList -I symbols<cr>", opts)
-- Do default action for next item
keyset("n", "<space>j", ":<C-u>CocNext<cr>", opts)
-- Do default action for previous item
keyset("n", "<space>k", ":<C-u>CocPrev<cr>", opts)
-- Resume latest coc list
keyset("n", "<space>p", ":<C-u>CocListResume<cr>", opts)
