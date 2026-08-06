{
  description = "Generic 2d editor for games";

  inputs = {
    # ClanLib 1.0 (patched) targets nixos-unstable; keep Flexlay on the
    # same channel so package attributes (libxi, libxmu, …) resolve and
    # we do not mix ABIs between the two flakes.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Patched ClanLib 1.0 that builds against current nixpkgs
    # (pkg-config, libxi, libxmu, …).  Override locally if needed:
    #   nix build . --override-input clanlib path:/path/to/clanlib-1.0
    clanlib.url = "github:grumbel/clanlib-1.0/0088ab39846c83838c7e4ebf42b5d89816c5c29d";
    clanlib.inputs.nixpkgs.follows = "nixpkgs";
    clanlib.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, clanlib }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Use the default Ruby from nixpkgs.  The SWIG extension must be
        # loaded with the same interpreter that compiled it (launcher
        # scripts hard-code this path at install time).
        # Note: do not probe removed attrs like ruby_2_7/swig4 — on
        # nixos-unstable they exist as rename stubs that throw on eval.
        ruby = pkgs.ruby;
        swig = pkgs.swig;
      in {
        packages = rec {
          default = flexlay-classic;

          flexlay-classic = pkgs.stdenv.mkDerivation {
            pname = "flexlay-classic";
            version = "0.0.0";

            src = nixpkgs.lib.cleanSource ./.;

            # Force the installed launcher scripts to invoke the same
            # Ruby that the SWIG extension was compiled against.  Using
            # a bare "ruby" picks up whatever is on $PATH (often a much
            # newer version) and produces:
            #   wrong argument type nil (expected Data) (TypeError)
            installPhase = ''
              make install PREFIX=$out RUBY=${ruby}/bin/ruby
            '';

            nativeBuildInputs = [
              pkgs.scons
              pkgs.pkg-config
            ];

            buildInputs = [
              clanlib.packages.${system}.default

              pkgs.libGL
              pkgs.libGLU
              swig
              ruby
              pkgs.zlib
            ];
          };
        };

        # Runnable front-ends: nix run .#<name>
        apps = let
          pkg = self.packages.${system}.default;
          mkApp = name: {
            type = "app";
            program = "${pkg}/bin/${name}";
          };
        in {
          default = mkApp "flexlay-paint";
          flexlay-paint = mkApp "flexlay-paint";
          netpanzer-editor = mkApp "netpanzer-editor";
          supertux-editor = mkApp "supertux-editor";
          supertux-portable = mkApp "supertux-portable";
        };
      }
    );
}
