# Configuration

  - [UCM Configuration](#ucm-configuration)
      - [`UNISON_DEBUG`](#unison_debug)
      - [`UNISON_PAGER`](#unison_pager)
      - [`UNISON_LSP_PORT`](#unison_lsp_port)
      - [`UNISON_LSP_ENABLED`](#unison_lsp_enabled)
      - [`UNISON_SHARE_HOST`](#unison_share_host)
      - [`UNISON_SHARE_ACCESS_TOKEN`](#unison_share_access_token)
      - [`UNISON_READONLY`](#unison_readonly)
      - [`UNISON_ENTITY_VALIDATION`](#unison_entity_validation)
      - [`UNISON_SYNC_VERSION`](#unison_sync_version)
      - [Local Codebase Server](#local-codebase-server)
  - [Codebase Configuration](#codebase-configuration)

## UCM Configuration

### `UNISON_DEBUG`

Enable debugging output for various portions of the application.
See `lib/unison-prelude/src/Unison/Debug.hs` for the full list of supported flags.

E.g.

``` sh
# Enable ALL debugging flags (likely quite noisy)
$ UNISON_DEBUG= ucm
# Enable timing debugging, printing how long different actions take.
$ UNISON_DEBUG=timing ucm
# Enable LSP and TIMING debugging
$ UNISON_DEBUG=lsp,timing ucm
```

### `UNISON_PAGER`

Allows selecting which pager to use for long command outputs.
Defaults to `less` on Linux & Mac, `more` on Windows

E.g.

``` sh
# User more instead of less
$ UNISON_PAGER=more ucm
```

### `UNISON_LSP_PORT`

Allows selecting the port to run the LSP server on. Defaults to `5757`.

E.g.

``` sh
$ UNISON_LSP_PORT=8080 ucm
```

### `UNISON_LSP_ENABLED`

Allows explicitly enabling or disabling the LSP server.
Acceptable values are 'true' or 'false'

Note for Windows users: Due to an outstanding issue with GHC's IO manager on Windows, the LSP is **disabled by default** on Windows machines.
Enabling the LSP on windows can cause UCM to hang on exit and may require the process to be killed by the operating system or via Ctrl-C.
Note that this doesn't pose any risk of codebase corruption or cause any known issues, it's simply an annoyance.

If you accept this annoyance, you can enable the LSP server on Windows by exporting the `UNISON_LSP_ENABLED=true` environment variable.

You can set this persistently in powershell using:

``` powershell
[System.Environment]::SetEnvironmentVariable('UNISON_LSP_ENABLED','true')
```

See [this issue](https://github.com/unisonweb/unison/issues/3487) for more details.

E.g.

``` sh
$ UNISON_LSP_ENABLED=true ucm
```

### `UNISON_SHARE_HOST`

Allows selecting the location for the default Share server.

E.g.

``` sh
$ UNISON_SHARE_HOST="http://localhost:5424" ucm
```

### `UNISON_SHARE_ACCESS_TOKEN`

Allows overriding the credentials used when authenticating with the Share server.

E.g.

``` sh
$ UNISON_SHARE_ACCESS_TOKEN="my.token.string" ucm
```

### `UNISON_READONLY`

Force unison to use readonly connections to codebases.

``` sh
$ UNISON_READONLY="true" ucm
```

### `UNISON_ENTITY_VALIDATION`

Allows disabling validation of entities pulled from a codebase server.
It's generally a good idea to leave this enabled unless you know exactly what you're doing.

Defaults to enabled.

``` sh
$ UNISON_ENTITY_VALIDATION="false" ucm
```

### `UNISON_SYNC_VERSION`

Allows enabling the experimental Sync Version 2 protocol when downloading code from Share.

``` sh
$ UNISON_ENTITY_VALIDATION="2" ucm
```

### `UNISON_PULL_WORKERS`

Allows setting the number of workers to use when pulling from a codebase server.
Defaults to 5.

``` sh
$ UNISON_PULL_WORKERS=6 ucm
```

### `UNISON_PUSH_WORKERS`

Allows setting the number of workers to use when pushing to a codebase server.
Defaults to 1.

``` sh
$ UNISON_PULL_WORKERS=2 ucm
```

### `UNISON_SYNC_CHUNK_SIZE`

Allows setting the chunk size used in requests when syncing a codebase.
Defaults to 50.

``` sh
$ UNISON_SYNC_CHUNK_SIZE=100 ucm
```

### Local Codebase Server

The port, host and token to be used for the local codebase server can all be configured by providing environment
variables when starting `ucm`, using `UCM_PORT`, `UCM_HOST`, and `UCM_TOKEN`.

E.g.

``` sh
UCM_PORT=8080 UCM_HOST=localhost UCM_TOKEN=1234 ucm
```

## Codebase Configuration

Also, see the guide [here](https://www.unison-lang.org/learn/tooling/configuration/)

The following configuration options can be provided within the `.unisonConfig` file,
which exists within the codebase directory, or at `~/.unisonConfig` for your default codebase.

``` 
# Attach myself as author and use BSD license for all of my contributions
DefaultMetadata = [ ".metadata.authors.chrispenner"
                  , ".metadata.licenses.chrispenner" ]

# RemoteMapping allows mapping a path in the codebase to a specific location on share.
# Here I state that I want my .share namespace to push to .chrispenner.public
# Everything inside .share will be mapped accordingly, e.g. .share.foo will map to
# chrispenner.public.foo on share.
RemoteMapping {
  share = "chrispenner.public"
}
```
