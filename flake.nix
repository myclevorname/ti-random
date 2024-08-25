{
  description = "Implemenation of the TI-83+ RNG";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with import nixpkgs { inherit system; }; rec {
        packages.default = packages.ti-random;
        packages.ti-random = haskellPackages.mkDerivation {
          src = self;
          pname = "ti-random";
          version = "0.1.0.1";
          license = lib.licenses.mit;
          isLibrary = true;
          isExecutable = false;
        };
      }
    );
}
