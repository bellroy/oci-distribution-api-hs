{
  description = "A library to read from OCI registries.";

  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs =
    inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      src = ./.;
      supportedCompilers = [
        "ghc912"
        "ghc914"
      ];
      defaultCompiler = "ghc912";
    };
}
