{
  description = "Generic 2d editor for games";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";

    clanlib.url = "github:grumbel/clanlib-1.0";
    clanlib.inputs.nixpkgs.follows = "nixpkgs";
    clanlib.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, clanlib }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = rec {
          default = flexlay-classic;

          flexlay-classic = pkgs.stdenv.mkDerivation {
            pname = "flexlay-classic";
            version = "0.0.0";

            src = nixpkgs.lib.cleanSource ./.;

            installPhase = ''
              make install PREFIX=$out
            '';

            nativeBuildInputs = [
              pkgs.scons
              pkgs.pkgconfig
            ];

            buildInputs = [
              clanlib.packages.${system}.default

              pkgs.libGL
              pkgs.libGLU
              pkgs.swig2
              pkgs.ruby_2_7
              pkgs.zlib
            ];
           };
        };
      }
    );
}
