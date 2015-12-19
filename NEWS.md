# Flexlay release and development notes

## Flexlay 0.2.0 (WIP for SuperTux 0.5.0)

- Flexlay is now being developed by The SuperTux Team, primarily for SuperTux
  level editing
- Flexlay will follow [SemVer](http://semver.org/) (just as SuperTux does)
- Removed C++ and Ruby support altogether, Flexlay now uses only Python 3 with
  PyQt 4. Many features from old C++ editor are lost
- Added SuperTux level creation wizard

## Flexlay 0.1.0

- Split Flexlay into a C++ library and a scripting modules, Ruby and (very limited)
  Python support is now available
- Restructured the scripting bindings
- C++ object hierachy now completly available in scripting
- Switched build system from autohell to SCons
- Support for resizable rectangular objects
- Better support for sprite objects
- Support for constrained object movement
- Major GUI cleanup
- An icon-toolbar
- Configurable keyboard shortcut support
- SuperTux: support for multiple sublevels and most features of lasted development
  version
- NetPanzer: support for object based level building, no more need to mess with
  raw tiles
- Pingus: basic support for loading levels and saving them, however only limited
  editing capabilties
- Paint: a simple paint application with support for animation

## Flexlay 0.0.1

- First release
- Zoom support
- Multilayer tilemap support
- Tilemap to png converting
- Tile brushes
- Simple copy/paste
- Undo/redo
- Game object support
- SuperTux support
- netPanzer support
- Windstille support
- Pingus support (load only)
