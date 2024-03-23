{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = with pkgs; with elmPackages; mkShell {
          buildInputs = [
            elm
            elm-format
            elm-test
            elm-review
            elm-json
          ];
        };
      });
}
