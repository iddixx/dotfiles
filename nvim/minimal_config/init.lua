
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
    { modes = { "n", "x" },      lhs = "a",          rhs = "b",      desc = "Word back" },
    { modes = { "n", "x" },      lhs = "t",          rhs = "w",      desc = "Word forward" },
    { modes = { "n", "v" },      lhs = "A",          rhs = "B",      desc = "WORD back" },
    { modes = { "n", "v" },      lhs = "T",          rhs = "W",      desc = "WORD forward" },

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
    { modes = { "n", "o", "x" },      lhs = "c",          rhs = "\"+c" },
    { modes = { "n", "o", "x" },      lhs = "dc",         rhs = "\"_x" },
    { modes = { "n", "o", "x" },      lhs = "dC",         rhs = "\"_X" },
    { modes = { "n", "o", "x" },      lhs = "dd",         rhs = "\"_dd" },

    -- Surround text --

    -- Surround with brackets
    { modes = { "n", "o", "x" },      lhs = "siw)",       rhs = "ciw()<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw)",       rhs = "caw()<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW)",       rhs = "ciW()<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW)",       rhs = "caW()<Esc>P" },
    { modes = { "v" },                lhs = "s)",         rhs = "c()<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siw(",       rhs = "ciw()<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw(",       rhs = "caw()<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW(",       rhs = "ciW()<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW(",       rhs = "caW()<Esc>P" },
    { modes = { "v" },                lhs = "s(",         rhs = "c()<Esc>P" },

    -- Surround with curly braces
    { modes = { "n", "o", "x" },      lhs = "siw}",       rhs = "ciw{}<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw}",       rhs = "caw{}<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW}",       rhs = "ciW{}<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW}",       rhs = "caW{}<Esc>P" },
    { modes = { "v" },                lhs = "s}",         rhs = "c{}<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siw{",       rhs = "ciw{}<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw{",       rhs = "caw{}<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW{",       rhs = "ciW{}<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW{",       rhs = "caW{}<Esc>P" },
    { modes = { "v" },                lhs = "s{",         rhs = "c{}<Esc>P" },

    -- Surround with square braces
    { modes = { "n", "o", "x" },      lhs = "siw]",       rhs = "ciw[]<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw]",       rhs = "caw[]<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW]",       rhs = "ciW[]<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW]",       rhs = "caW[]<Esc>P" },
    { modes = { "v" },                lhs = "s]",         rhs = "c[]<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siw[",       rhs = "ciw[]<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw[",       rhs = "caw[]<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW[",       rhs = "ciW[]<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW[",       rhs = "caW[]<Esc>P" },
    { modes = { "v" },                lhs = "s[",         rhs = "c[]<Esc>P" },

    -- Surround with angle brackets
    { modes = { "n", "o", "x" },      lhs = "siw>",       rhs = "ciw<><Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw>",       rhs = "caw<><Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW>",       rhs = "ciW<><Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW>",       rhs = "caW<><Esc>P" },
    { modes = { "v" },                lhs = "s>",         rhs = "c<><Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siw<",       rhs = "ciw<><Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw<",       rhs = "caw<><Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW<",       rhs = "ciW<><Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW<",       rhs = "caW<><Esc>P" },
    { modes = { "v" },                lhs = "s<",         rhs = "c<><Esc>P" },

    -- Surround with single quotes
    { modes = { "n", "o", "x" },      lhs = "siw\'",       rhs = "ciw\'\'<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw\'",       rhs = "caw\'\'<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW\'",       rhs = "ciW\'\'<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW\'",       rhs = "caW\'\'<Esc>P" },
    { modes = { "v" },                lhs = "s\'",         rhs = "c\'\'<Esc>P" },

    -- Surround with double quotes
    { modes = { "n", "o", "x" },      lhs = "siw\"",       rhs = "ciw\"\"<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saw\"",       rhs = "caw\"\"<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "siW\"",       rhs = "ciW\"\"<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "saW\"",       rhs = "caW\"\"<Esc>P" },
    { modes = { "n", "o", "x" },      lhs = "ss\"",        rhs = "cc\"\"<Esc>P" },
    { modes = { "v" },                lhs = "s\"",         rhs = "c\"\"<Esc>P" },

    -- fixes
    { modes = { "n", "o", "x" },      lhs = "ci",         rhs = "\"+ci" },
    { modes = { "n", "o", "x" },      lhs = "cc",         rhs = "\"+cc" },
    { modes = { "n", "o", "x" },      lhs = "yi",         rhs = "\"+yi" },
    { modes = { "n", "o", "x" },      lhs = "yy",         rhs = "\"+yy" },
    { modes = { "n", "o", "x" },      lhs = "vi",         rhs = "vi" },
    { modes = { "n", "o", "x" },      lhs = "di",         rhs = "\"_di" },
    { modes = { "n", "o", "x" },      lhs = "ca",         rhs = "ca" },
    { modes = { "n", "o", "x" },      lhs = "ya",         rhs = "\"+ya" },
    { modes = { "n", "o", "x" },      lhs = "va",         rhs = "va" },
    { modes = { "n", "o", "x" },      lhs = "da",         rhs = "\"_da" },
    { modes = { "n", "o", "x" },      lhs = "gd",         rhs = "gd" },

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
    { modes = { "n", "o", "x" },      lhs = "k",          rhs = "n" },
    { modes = { "n", "o", "x" },      lhs = "K",          rhs = "N" },

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
    { modes = { "n" },                lhs = "<M-d>",      rhs = "<cmd>lua vim.diagnostic.open_float()<CR>" },
    { modes = { "n" },                lhs = ";",          rhs = "q:" },
    { modes = { "n", "v", "o", "x" }, lhs = "g|",         rhs = "g~" },
}

function colemak_apply()
    for _, mapping in pairs(colemak_mappings) do
        vim.keymap.set(
            mapping.modes,
            mapping.lhs,
            mapping.rhs,
            { desc = mapping.desc }
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

--[[ Editor ]]
vim.g.loaded_netrw       = 1
vim.g.loaded_netrwPlugin = 1
vim.cmd("syntax on")
vim.cmd("filetype on")
vim.cmd("filetype indent on")
vim.cmd("set t_Co=256")
vim.cmd("set showcmd!")
-- vim.cmd("au VimLeave * set guicursor=a:hor100") -- for underline
vim.cmd("au VimLeave * set guicursor=a:ver100") -- for beam cursor
-- vim.cmd("au VimLeave * set guicursor=a:block" ) -- for block(you maybe wont need it)

vim.cmd("command B buffers")
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
--vim.cmd("set noshowmode")

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
vim.opt.termguicolors = true
vim.opt.background = "dark"
