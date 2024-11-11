# Unison Language Server

[![asciicast](https://asciinema.org/a/Kwa7NscffA3R8KCHxq1OavRm0.svg)](https://asciinema.org/a/Kwa7NscffA3R8KCHxq1OavRm0)

  - [Overview](#overview)
  - [Installation and setup](#installation-and-setup)
      - [NeoVim](#neovim)
      - [VSCode](#vscode)
      - [Helix Editor](#helix-editor)
      - [Emacs](#emacs)
      - [other editors](#other-editors)
  - [Configuration](#configuration)

## Overview

Supported features:

  - Autocompletion
  - Inline type and parser error messages
  - Format on save (you can disable this in your editor if you like)
  - Show type on hover

Notes:

  - The LSP listens for changes from the UCM it's linked to, so name resolution is dependent on your current UCM path.

## Installation and setup

Currently the only supported configuration is to connect to the LSP via a specified port, not all LSP implementations support this configuration.

By default the LSP is hosted at `127.0.0.1:5757`, but you can change the port using `UNISON_LSP_PORT=1234`.

Note for Windows users: Due to an outstanding issue with GHC's IO manager on Windows, the LSP is **disabled by default** on Windows machines.
Enabling the LSP on windows can cause UCM to hang on exit and may require the process to be killed by the operating system or via Ctrl-C.
Note that this doesn't pose any risk of codebase corruption or cause any known issues, it's simply an annoyance.

If you accept this annoyance, you can enable the LSP server on Windows by exporting the `UNISON_LSP_ENABLED=true` environment variable.

You can set this persistently in powershell using:

``` powershell
[System.Environment]::SetEnvironmentVariable('UNISON_LSP_ENABLED','true')
```

See [this issue](https://github.com/unisonweb/unison/issues/3487) for more details.

### NeoVim

Before configuring the LSP, install the Vim plugin for filetype detection and syntax highlighting.
For [Packer](https://github.com/wbthomason/packer.nvim) you can install the package as follow:

``` lua
-- You may need to increase the git clone timeout setting in Packer!
use {
  "unisonweb/unison",
  branch = "trunk",
  rtp = "/editor-support/vim"
}
```

or [Plug](https://github.com/junegunn/vim-plug):

``` vim
Plug 'unisonweb/unison', { 'branch': 'trunk', 'rtp': 'editor-support/vim' }
```

or [Lazy](https://github.com/folke/lazy.nvim/):

``` lua
{
  "unisonweb/unison",
  branch = "trunk",
  config = function(plugin)
      vim.opt.rtp:append(plugin.dir .. "/editor-support/vim")
      require("lazy.core.loader").packadd(plugin.dir .. "/editor-support/vim")
  end,
  init = function(plugin)
       require("lazy.core.loader").ftdetect(plugin.dir .. "/editor-support/vim")
  end,
}
```

Configuration for [coc-nvim](https://github.com/neoclide/coc.nvim), enter the following in the relevant place of your CocConfig

``` 
  "languageserver": {
    "unison": {
      "filetypes": ["unison"],
      "host": "127.0.0.1",
      "port": 5757,
      "settings": {}
    }
  }
```

For [lspconfig](https://github.com/neovim/nvim-lspconfig) with optional autocomplete [nvim-cmp](https://github.com/hrsh7th/nvim-cmp) for LSP
[cmp-nvim-lsp](https://github.com/hrsh7th/cmp-nvim-lsp), you can use the following setup function(s):

``` lua
-- This function is for configuring a buffer when an LSP is attached
local on_attach = function(client, bufnr)
  -- Always show the signcolumn, otherwise it would shift the text each time
  -- diagnostics appear/become resolved
  vim.o.signcolumn = 'yes'

  -- Update the cursor hover location every 1/4 of a second
  vim.o.updatetime = 250

  -- Disable appending of the error text at the offending line
  vim.diagnostic.config({virtual_text=false})

  -- Enable a floating window containing the error text when hovering over an error
  vim.api.nvim_create_autocmd("CursorHold", {
    buffer = bufnr,
    callback = function()
      local opts = {
        focusable = false,
        close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
        border = 'rounded',
        source = 'always',
        prefix = ' ',
        scope = 'cursor',
      }
      vim.diagnostic.open_float(nil, opts)
    end
  })

  -- This setting is to display hover information about the symbol under the cursor
  vim.keymap.set('n', 'K', vim.lsp.buf.hover)

end

-- Setup the Unison LSP
require('lspconfig')['unison'].setup{
    on_attach = on_attach,
}
```

``` lua
-- This is NVim Autocompletion support
local cmp = require 'cmp'

-- This function sets up autocompletion
cmp.setup {

  -- This mapping affects the autocompletion choices menu
  mapping = cmp.mapping.preset.insert(),

  -- This table names the sources for autocompletion
  sources = {
    { name = 'nvim_lsp' },
  },
}

```

Note that you'll need to start UCM *before* you try connecting to it in your editor or your editor might give up.

### VSCode

Simply install the [Unison Language VSCode extension](https://marketplace.visualstudio.com/items?itemName=unison-lang.unison).

### Helix Editor

To `~/.config/helix/languages.toml` append this code:

``` toml
[language-server.ucm]
command = "nc" # or 'ncat' or 'netcat'
args = ["localhost", "5757"]

[[language]]
name = "unison"
scope = "source.unison"
injection-regex = "unison"
file-types = ["u"]
shebangs = []
roots = []
auto-format = false
comment-token = "--"
indent = { tab-width = 4, unit = "    " }
language-servers = [ "ucm" ]

```

or follow the instructions for Unison in "[How to install the default language servers](https://github.com/helix-editor/helix/wiki/How-to-install-the-default-language-servers#unison)" wiki page.

### Emacs

In Emacs 29 (or earlier, if you install the [Eglot](https://elpa.gnu.org/packages/eglot.html) package), add the following to your init file:

``` elisp
(push '((unison-ts-mode unisonlang-mode) "127.0.0.1" 5757)
      eglot-server-programs)
```

This requires having either [unison-ts-mode](https://github.com/fmguerreiro/unison-ts-mode) or [unisonlang-mode](https://melpa.org/#/unisonlang-mode) installed. unison-ts-mode is newer, supported, and more complete, but isn’t in [MELPA](https://melpa.org/) yet and requires a couple commands to set up [tree-sitter-unison](https://github.com/kylegoetz/tree-sitter-unison).

You can then use `M-x eglot` in a Unison scratch file buffer. You can also [configure Eglot to start automatically](https://www.gnu.org/software/emacs/manual/html_node/eglot/Starting-Eglot.html).

### Other Editors

If your editor provides a mechanism for connecting to a host and port, provide a host of `127.0.0.1` and port `5757`.

If your editor requires a command to run, you can provide the command `nc localhost 5757` on Mac, or `netcat localhost 5757` on linux.
Note that some editors require passing the command and arguments as separate parameters.

## Configuration

Supported settings and their defaults. See information for your language server client about where to provide these.

  - `formattingWidth`: A suggestion for the formatter about how wide (in columns) to print definitions.

  - `maxCompletions`: The number of completions the server should collect and send based on a single query.  Increasing this limit will provide more completion results, but at the cost of being slower to respond.
    
    If explicitly set to `null` the server will return ALL completions available.

``` json
{
  "formattingWidth": 80,
  "maxCompletions": 100
}
```
