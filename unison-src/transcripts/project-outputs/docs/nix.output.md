(Todo: it might be nice to have a gentle and concise introduction into why Unison does anything with Nix and what cachix is.)

We can push any nix store path into our cachix cache. This is typically done with `cachix push unison <STORE_PATH>`.

Some ways to come up with a store path:

1.  If you build something you get a symlink to the store path of the built thing, named `result` by default.
2.  With `nix path-info`

So, you could push the unison executable with the cache with

``` nix
nix build -o my-little-unison-store-path
cachix push unison my-little-unison-store-path
```

or

``` nix
nix build | cachix push unison
```

We want to cache the \[immediate\] build dependencies of our build products, because those are the only ones actually needed to build our build products.

``` nix
nix-store --query --references $(nix path-info --derivation) | xargs nix-store --realize | cachix push unison
```

Breaking down the above:

``` nix
nix path-info --derivation
```

gets the store path of the derivation of the unison executable

``` nix
nix-store --query --references $(nix path-info --derivation)
```

gets the store paths of the derivations of immediate dependencies of the unison executable derivation.

``` nix
nix-store --query --references $(nix path-info --derivation) | xargs nix-store --realize
```

builds the above derivations if necessary and writes the resulting store paths to stdout

These paths are then fed to cachix with `| cachix push unison`.

Development environments are defined in the flake under the `devShells` key. There are a number of different development environments, and they can be entered by giving a different argument to `nix develop`. If you want to push a development environment you could do so with something like:

``` nix
nix build --no-link '.#devShells.x86_64-linux.default' | cachix push unison
```

and you could push the build dependencies of the default shell with something like

``` nix
nix-store --query --references $(nix path-info --derivation '.#devShells.x86_64-linux.default') | xargs nix-store --realize | cachix push unison
```

``` nix
nix-store --query --references $(nix path-info --derivation '.#devShells.aarch64-darwin.default') | xargs nix-store --realize | cachix push unison
```
