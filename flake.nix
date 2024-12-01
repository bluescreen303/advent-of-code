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
    with nixpkgs.lib;
    flake-utils.lib.eachSystem ["x86_64-linux"] (system:
      let pkgs = bluenix.legacyPackages.${system};
          hp = pkgs.haskellPackages;
      in
      {
        packages = {
          aoc-hs = hp.callPackage aoc-hs/advent-of-code.nix {};
          aoc-rs = let src = ./aoc-rs;
                     cargoTOML = (fromTOML (readFile (src + /Cargo.toml)));
                     pname     = cargoTOML.package.name;
                     version   = cargoTOML.package.version;
                   in
                     pkgs.callPackage ({ inShell ? false }: pkgs.rustPlatform.buildRustPackage {
                       inherit pname version src;

                       cargoLock.lockFile = src + /Cargo.lock;

                       nativeBuildInputs = optionals inShell [
                         pkgs.rustfmt
                         pkgs.clippy
                         pkgs.rust-analyzer
                       ];
                     }) {};
          default = self.packages.${system}.aoc-rs;
        };
        devShells = {
          aoc-hs = bluenix.lib.haskell-shell hp self.packages.${system}.aoc-hs;
          aoc-rs = self.packages.${system}.aoc-rs.override { inShell = true; };
          aoc-jq = pkgs.mkShell {
            name = "aoc-jq-shell";
            packages = [ pkgs.jaq ];
          };
          default = self.devShells.${system}.aoc-rs;
        };
      }
    );
}
