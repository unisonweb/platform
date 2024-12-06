{
  description = "Unison";

  nixConfig = {
    extra-substituters = ["https://unison.cachix.org"];
    extra-trusted-public-keys = ["unison.cachix.org-1:i1DUFkisRPVOyLp/vblDsbsObmyCviq/zs6eRuzth3k="];
  };

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-2405";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    haskellNix,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]
    (system: let
      ## It’s much easier to read from a JSON file than to have JSON import from some other file, so we extract some
      ## configuration from the VS Code settings to avoid duplication.
      vscodeSettings = nixpkgs.lib.importJSON ./.vscode/settings.json;
      versions =
        vscodeSettings."haskell.toolchain"
        ## There are some things we want to pin that the VS Code Haskell extension doesn’t let us control.
        // {
          hpack = "0.36.0";
          ormolu = "0.7.2.0";
        };
      pkgs = import nixpkgs {
        inherit system;
        inherit (haskellNix) config;
        overlays = [
          haskellNix.overlay
          (import ./nix/dependencies.nix)
        ];
      };
      unison-project = import ./nix/unison-project.nix {
        inherit (nixpkgs) lib;
        inherit (pkgs) haskell-nix;
      };
      haskell-nix-flake = import ./nix/haskell-nix-flake.nix {
        inherit pkgs unison-project versions;
        inherit (nixpkgs) lib;
      };
      renameAttrs = fn:
        nixpkgs.lib.mapAttrs' (name: value: {
          inherit value;
          name = fn name;
        });
    in
      assert pkgs.stack.version == versions.stack;
      assert pkgs.hpack.version == versions.hpack; {
        packages =
          renameAttrs (name: "component-${name}") haskell-nix-flake.packages
          // renameAttrs (name: "docker-${name}") (import ./nix/docker.nix {
            inherit pkgs;
            haskell-nix = haskell-nix-flake.packages;
          })
          // {
            default = haskell-nix-flake.defaultPackage;
            all = pkgs.symlinkJoin {
              name = "all";
              paths = let
                all-other-packages = builtins.attrValues (builtins.removeAttrs self.packages."${system}" [
                  "all"
                  "docker-ucm" # this package doesn’t produce a directory
                ]);
                devshell-inputs =
                  builtins.concatMap
                  (devShell: devShell.buildInputs ++ devShell.nativeBuildInputs)
                  (builtins.attrValues self.devShells."${system}");
              in
                all-other-packages ++ devshell-inputs;
            };
          };

        apps =
          renameAttrs (name: "component-${name}") haskell-nix-flake.apps
          // {default = self.apps."${system}"."component-unison-cli-main:exe:unison";};

        devShells =
          renameAttrs (name: "cabal-${name}") haskell-nix-flake.devShells
          // {default = self.devShells."${system}".cabal-local;};

        checks = renameAttrs (name: "component-${name}") haskell-nix-flake.checks;

        formatter = pkgs.alejandra;
      });
}
