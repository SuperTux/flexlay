{
  description = "SuperTux 0.4.0 level editor";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages = flake-utils.lib.flattenTree rec {
          flexlay = pkgs.python3Packages.buildPythonPackage rec {
            name = "flexlay";
            src = self;
            nativeBuildInputs = [ pkgs.qt5.wrapQtAppsHook ];
            makeWrapperArgs = [
              "\${qtWrapperArgs[@]}"
              "--set" "LIBGL_DRIVERS_PATH" "${pkgs.mesa.drivers}/lib/dri"
              "--prefix" "LD_LIBRARY_PATH" ":" "${pkgs.mesa.drivers}/lib"
            ];
            preCheck = ''
              export QT_QPA_PLATFORM_PLUGIN_PATH="${pkgs.qt5.qtbase.bin}/lib/qt-${pkgs.qt5.qtbase.version}/plugins";
            '';
            propagatedBuildInputs = [
              pkgs.xorg.libxcb
              pkgs.p7zip
              pkgs.python3Packages.setuptools
              pkgs.python3Packages.numpy
              pkgs.python3Packages.pyqt5
              pkgs.python3Packages.pyxdg
            ];
          };
        };
        defaultPackage = packages.flexlay;
      });
}
