# AGENTS.md — Flexlay Project Overview

## Project Summary

**Flexlay** is a generic 2D game editor focused on level creation for 2D games.  
It provides multi-layered tilemaps, object layers, bitmap layers, full undo/redo, tile brushes, copy/paste, multiple buffers, minimap support, and a metadata editor.

Flexlay itself is a reusable C++ library + scripting layer. Game-specific editors are thin wrappers that load Flexlay and configure it for a particular game’s data formats and assets.

**License:** GNU General Public License v3 (or later)  
**Primary author:** Ingo Ruhnke <grumbel@gmx.de>

## Supported Games / Front-ends

| Directory            | Purpose                                      |
|----------------------|----------------------------------------------|
| `supertux/`          | SuperTux level & world-map editor            |
| `supertux-portable/` | Portable / simplified SuperTux editor        |
| `netpanzer/`         | netPanzer level editor (object-based)        |
| `paint/`             | Simple paint / animation application with tablet support |

Older experimental support for Pingus and Windstille is mentioned in history but not present as first-class front-ends in this tree.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  Game-specific Ruby scripts (supertux-editor, …)        │
└────────────────────────────┬────────────────────────────┘
                             │ uses
┌────────────────────────────▼────────────────────────────┐
│  Ruby bindings (SWIG-generated + hand-written helpers)  │
│  ruby/                                                  │
└────────────────────────────┬────────────────────────────┘
                             │ wraps
┌────────────────────────────▼────────────────────────────┐
│  Flexlay C++ core library                               │
│  lib/  (tilemaps, object layers, tools, GUI widgets,    │
│         commands, brushes, workspace, …)                │
└────────────────────────────┬────────────────────────────┘
                             │ depends on
┌────────────────────────────▼────────────────────────────┐
│  ClanLib 1.0 (Display, GL, GUI, Signals, Core)          │
└─────────────────────────────────────────────────────────┘
```

### Key C++ components (`lib/`)

- **Workspace / EditorMap** — central map state, layers, undo stack
- **Layers** — `TilemapLayer`, `ObjectLayer`, `BitmapLayer`, `SketchLayer`, `OnionSkinLayer`
- **Tools** — paint, select, zoom, object select, workspace move, sketch stroke, …
- **Commands** — undoable operations (`PaintCommand`, `ObjectAddCommand`, …)
- **GUI** — custom widgets built on ClanLib GUI (tile selector, minimap, menus, dialogs)
- **Brushes** — tile brushes and generated brushes
- **Metadata** — flexible key/value storage attached to map objects

### Scripting layer (`ruby/`)

- SWIG interface (`flexlay_wrap.i`) exposes most of the C++ API
- Helper modules for S-expression I/O, config files, GUI layout, etc.
- Game editors are written almost entirely in Ruby and call into the wrapped C++ library

## Build System

- **Primary:** SCons (`SConstruct` + per-directory `SConscript`)
- **Nix flake** (`flake.nix`) for reproducible builds (ClanLib 1.0, SWIG 2, Ruby 2.7)
- Requires working OpenGL / hardware 3D acceleration

Typical build:

```bash
scons
```

Or via Nix:

```bash
nix build
```

## Data & Assets

- `data/` — shared GUI skin, icons (16×16 & 24×24), fonts, brushes
- Game-specific assets live inside each front-end directory (e.g. `netpanzer/netpanzersprites/`, thumbnails)

## Development Notes

- Flexlay cannot be launched by itself; always start a game wrapper script.
- Paths to game data are often hardcoded in the Ruby scripts and may need adjustment.
- The project is relatively old (ClanLib 1.0 era) and targets Ruby 2.x / SWIG 1.3–2.x.
- Undo/redo, multi-layer editing, and brush-based painting are core strengths.

## Useful Entry Points for Agents

1. Start reading `lib/flexlay.hpp` and `lib/workspace.hpp` for the core API.
2. Look at `supertux/supertux.rb` or `netpanzer/netpanzer.rb` to see how a front-end is structured.
3. GUI layout and keybindings are defined in the Ruby side (`gui_specs.rb`, etc.).
4. Tile / object selection and painting logic lives under `lib/tools/` and `lib/gui/`.

---

*This file is intended as a quick orientation document for automated agents and new contributors.*
