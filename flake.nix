{
  description = "idris2-base64";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    idris2-pkgs = {
      url = "github:claymager/idris2-pkgs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "utils";
        idris-server.follows = "";
      };
    };
  };

  outputs = { self, nixpkgs, utils, idris2-pkgs }: {
    overlays = rec {
      expects-idris2-pkgs = final: _: {
        idris2-base64 = final.idris2-pkgs._builders.idrisPackage ./. { };
      };

      default = final: prev: {
        inherit (prev.appendOverlays [
          idris2-pkgs.overlay
          expects-idris2-pkgs
        ]) idris2-base64;
      };
    };
  } // utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        overlays = [ idris2-pkgs.overlay self.overlays.expects-idris2-pkgs ];
        inherit system;
      };
      inherit (pkgs.idris2-pkgs._builders) devEnv;
    in
    with pkgs; {
      packages.default = idris2-base64;

      devShells.default = mkShell {
        packages = [
          (devEnv (pkgs.idris2-pkgs._builders.idrisPackage ./test { }))
        ];
      };
    });
}
