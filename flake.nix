{
  description = "SuperTux 0.4.0 level editor";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pythonPackages = pkgs.python310Packages;
      in rec {
        packages = flake-utils.lib.flattenTree rec {

          PyQt5-stubs = pythonPackages.buildPythonPackage rec {
            pname = "PyQt5-stubs";
            version = "5.15.6.0";
            src = pythonPackages.fetchPypi {
              inherit pname version;
              sha256 = "sha256-kScKwj6/OKHcBM2XqoUs0Ir4Lcg5EA5Tla8UR+Pplwc=";
            };
          };

          flexlay = pythonPackages.buildPythonPackage rec {
            pname = "flexlay";
            version = "0.2.0";
            src = ./.;
            nativeBuildInputs = [ pkgs.qt5.wrapQtAppsHook ];
            makeWrapperArgs = [
              "\${qtWrapperArgs[@]}"
              "--set" "LIBGL_DRIVERS_PATH" "${pkgs.mesa.drivers}/lib/dri"
              "--prefix" "LD_LIBRARY_PATH" ":" "${pkgs.mesa.drivers}/lib"
            ];
            preCheck = ''
              export QT_QPA_PLATFORM_PLUGIN_PATH="${pkgs.qt5.qtbase.bin}/lib/qt-${pkgs.qt5.qtbase.version}/plugins";
            '';
            shellHook = ''
              export QT_QPA_PLATFORM_PLUGIN_PATH="${pkgs.qt5.qtbase.bin}/lib/qt-${pkgs.qt5.qtbase.version}/plugins";
            '';
            checkPhase = ''
              runHook preCheck
              flake8 flexlay supertux tests
              # pyright flexlay supertux tests
              # mypy flexlay supertux tests
              # pylint flexlay supertux tests
              # python3 -m unittest discover -v -s tests/
              runHook postCheck
            '';
            propagatedBuildInputs = [
              pkgs.xorg.libxcb
              pythonPackages.setuptools
              pythonPackages.numpy
              pythonPackages.pyqt5
              pythonPackages.pyxdg
            ];
            checkInputs = (with pkgs; [
              pyright
            ]) ++ (with pythonPackages; [
              flake8
              mypy
              pylint
              types-setuptools
              pyannotate
            ]) ++ [
              PyQt5-stubs
            ];
          };
          default = flexlay;
        };
      }
    );
}
