{
  description = "SuperTux 0.4.0 level editor";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pythonPackages = pkgs.python3Packages;
      in rec {
        packages = rec {

          PyQt5-stubs = pythonPackages.buildPythonPackage rec {
            pname = "PyQt5-stubs";
            version = "5.15.6.0";
            src = pythonPackages.fetchPypi {
              inherit pname version;
              sha256 = "sha256-kScKwj6/OKHcBM2XqoUs0Ir4Lcg5EA5Tla8UR+Pplwc=";
            };
            pyproject = true;
            build-system = [ pythonPackages.setuptools ];
          };

          flexlay = pythonPackages.buildPythonPackage rec {
            pname = "flexlay";
            version = "0.2.0";
            src = ./.;
            pyproject = true;
            build-system = [ pythonPackages.setuptools ];
            nativeBuildInputs = [
              pkgs.qt5.wrapQtAppsHook
            ];

            nativeCheckInputs = [
              pythonPackages.flake8  # FIXME: not sure why this is needed here
            ];

            makeWrapperArgs = [
              "\${qtWrapperArgs[@]}"
              "--set" "LIBGL_DRIVERS_PATH" "${pkgs.mesa.drivers}/lib/dri"
              "--prefix" "LD_LIBRARY_PATH" ":" "${pkgs.mesa.drivers}/lib"
            ];
            preCheck = ''
              export QT_QPA_PLATFORM_PLUGIN_PATH="${pkgs.qt5.qtbase.bin}/lib/qt-${pkgs.qt5.qtbase.version}/plugins";
              # export QT_QPA_PLATFORM=offscreen
            '';
            checkPhase = ''
              runHook preCheck
              flake8 flexlay supertux tests
              # pyright flexlay supertux tests
              # mypy flexlay supertux tests
              # pylint flexlay supertux tests
              # HOME=$TMP python3 -m unittest discover -v -s tests/
              runHook postCheck
            '';
            propagatedBuildInputs = [
              pkgs.xorg.libxcb
              pythonPackages.setuptools
              pythonPackages.numpy
              pythonPackages.pyqt5
              pythonPackages.pyqt5-sip
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

          flexlay-nocheck = flexlay.override {
            doCheck = false;
          };

          default = flexlay;
        };

        devShells = rec {
          flexlay-dev = pkgs.mkShell {
            inputsFrom = [ packages.flexlay ];
            shellHook = packages.flexlay.preCheck + ''
              # runHook setuptoolsShellHook
            '';
          };
          default = flexlay-dev;
        };
      }
    );
}
