{
  description = "advent of code flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    bluenix.url = "git+http://mathijs@git.lan/bluenix";
    bluenix.inputs.nixpkgs.follows = "nixpkgs";
    bluenix.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, bluenix }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let pkgs = bluenix.legacyPackages.${system};
          hp = pkgs.haskellPackages;
      in
      {
        packages.default = hp.callPackage ./advent-of-code.nix {};
        devShells.default = bluenix.lib.haskell-shell hp self.packages.${system}.default;
      }
    );
}
