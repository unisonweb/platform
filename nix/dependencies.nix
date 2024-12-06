final: prev: {
  ## See https://docs.haskellstack.org/en/stable/nix_integration/#supporting-both-nix-and-non-nix-developers for an
  ## explanation of this derivation.
  stack = final.symlinkJoin {
    inherit (prev.stack) version;
    name = "stack";
    paths = [prev.stack];
    buildInputs = [final.makeWrapper];
    postBuild = ''
      wrapProgram $out/bin/stack --add-flags "--no-nix --system-ghc --no-install-ghc"
    '';
  };
}
