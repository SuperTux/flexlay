# AGENTS.md — Flexlay Project Overview

## Project Summary

**Flexlay** is a generic 2D game editor focused on level creation for 2D games.
It provides multi-layered tilemaps, object layers, bitmap layers, full undo/redo, tile brushes, copy/paste, multiple buffers, minimap support, and a metadata editor.

Flexlay itself is a reusable C++ library + scripting layer. Game-specific editors are thin wrappers that load Flexlay and configure it for a particular game’s data formats and assets.

**License:** GNU General Public License v3 (or later)
**Primary author:** Ingo Ruhnke <grumbel@gmx.de>

## Supported Games / Front-ends

| Directory            | Binary (Nix app)      | Status (nixos-unstable / Ruby 3.4 / modern SWIG) |
|----------------------|-----------------------|--------------------------------------------------|
| `paint/`             | `flexlay-paint`       | **Works** — window opens, basic GUI runs |
| `netpanzer/`         | `netpanzer-editor`    | **In progress** — GUI starts; layer set_current + data path fixes ongoing |
| `supertux/`          | `supertux-editor`     | **Installed** — shares core Ruby/SWIG fixes; needs runtime testing |
| `supertux-portable/` | `supertux-portable`   | **Installed** — shares core Ruby/SWIG fixes; needs runtime testing |

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

- SWIG interface (`lib/flexlay_wrap.i`, `lib/clanlib.i`) exposes most of the C++ API
- Helper modules for S-expression I/O, config files, GUI layout, etc.
- Game editors are written almost entirely in Ruby and call into the wrapped C++ library

## Build System

- **Primary:** SCons (`SConstruct` + per-directory `SConscript`)
- **Nix flake** (`flake.nix`) on **nixos-unstable** with patched ClanLib 1.0
- Requires working OpenGL / hardware 3D acceleration (or `LIBGL_ALWAYS_SOFTWARE=1`)

```bash
nix build
nix run .#flexlay-paint
nix run .#netpanzer-editor
```

Or via SCons / Makefile with an explicit Ruby:

```bash
make install PREFIX=… RUBY=$(which ruby)
```

## Lessons learned: porting to modern Ruby + SWIG

The tree was written for **Ruby 2.x** and **SWIG 1.3/2.x**. On current nixos-unstable (Ruby 3.4, SWIG 4.x) the following issues appear and how they were fixed.

### 1. Launcher scripts must pin the build Ruby

Bare `exec ruby …` picks up whatever is on `$PATH`. The SWIG extension ABI must match the interpreter that compiled it.

- Makefile: `RUBY` variable, default `ruby`
- `flake.nix` `installPhase`: `make install … RUBY=${ruby}/bin/ruby`
- In-tree launcher scripts: `RUBY="${RUBY:-ruby}"`

Symptom otherwise: `wrong argument type nil (expected Data) (TypeError)`.

### 2. Do not probe removed nixpkgs attributes

On unstable, attrs like `swig4` / `ruby_2_7` still *exist* as rename stubs that **throw on eval**. Use plain `pkgs.swig` and `pkgs.ruby`. Prefer the same nixpkgs channel as the ClanLib flake (`inputs.follows`).

### 3. pkg-config for Ruby is versioned

`ruby/SConscript` and `netpanzer/SConscript` used to hardcode `ruby-2.7`. They now probe `ruby`, `ruby-3.x`, `ruby-2.7`, … (override with `RUBY_PKG`).

### 4. Bare `alias` on SWIG methods fails under Ruby 3.4

```ruby
class TilemapLayer
  alias orig_get_metadata get_metadata  # NameError on Ruby 3.4
```

Use `Module#prepend` + `super` instead (see `ruby/flexlay.rb` for `TilemapLayer`, `EditorMap`, `Menu`, etc.).

### 5. SWIG drops constructors for parent-owned GUI widgets

ClanLib-style widgets often have **private/protected destructors**. Modern SWIG then emits only a zero-arg `initialize`, so `Icon.new(rect, sprite, tip, parent)` fails with `wrong number of arguments (given N, expected 0)`.

Fixes used:

- Present a **public-dtor view** of the class in the `.i` file (do not copy `private: ~Foo()` into the SWIG interface).
- Mark parent-owned types with empty ownership features:
  ```swig
  %feature("ref")   Icon "";
  %feature("unref") Icon "";
  ```
- Enable `%feature("compactdefaultargs");` globally for overloaded ctors with default args.
- Declare incomplete types used in signatures (`CL_StyleManager`, `CL_Signal_v0`, …). **Do not** declare `CL_Signal_v1` as a non-template class (it is a template in ClanLib).
- Prefer **`%inline` free-function factories** over `%extend` static methods (the latter were not reliably visible as Ruby class methods):
  ```swig
  %newobject Icon_create;
  %inline %{
  Icon* Icon_create(const CL_Rect&, const CL_Sprite&, const std::string&, CL_Component*);
  %}
  ```
  Call via `Flexlay_wrap.Icon_create(...)` or `flexlay_factory(:Icon_create)` in Ruby.  
  Note: `defined?(Icon_create)` is **false** after `include Flexlay_wrap` (methods become private); use `Flexlay_wrap.respond_to?(:Icon_create)` or `respond_to?(:Icon_create, true)`.

### 6. Reopen SWIG classes under the module name

```ruby
class Icon
  def set_callback(...)  # may bind to the wrong constant
```

Prefer:

```ruby
class Flexlay_wrap::Icon
  def set_callback(func)
    connect(sig_clicked(), func)
  end
end
```

Same for `Flexlay_wrap::CL_Menu#add_item`, etc.

### 7. Static C++ methods may be omitted

`TilemapLayer.set_current` / `ObjectLayer.set_current` were missing. Re-export with `%extend` and, if needed, Ruby `define_singleton_method` fallbacks (Ruby 3.4 rejects `def Foo::Bar.baz` syntax).

### 8. OpenGL / exit-time crashes

- Legacy ClanLib GLX can segfault on context creation; try `LIBGL_ALWAYS_SOFTWARE=1`.
- Double-free of parent-owned widgets at Ruby finalizer time → empty `%feature("unref")` as above.

## Still TODO

- Runtime-test **supertux-editor** and **supertux-portable**; fix any remaining front-end-specific Ruby 3 issues.
- Quiet unused-variable warnings in front-end GUIs (`gui.rb` locals).
- netPanzer game data path (`/usr/share/games/netpanzer/`) is often absent; make it configurable.
- Broader SWIG coverage audit: any remaining `Foo.new(...)` that still hits “expected 0” needs a factory or interface fix.
- Exit-time segfaults from parent-owned widget finalizers (mitigated by empty `unref`, may still need work).

## Data & Assets

- `data/` — shared GUI skin, icons (16×16 & 24×24), fonts, brushes
- Game-specific assets live inside each front-end directory (e.g. `netpanzer/netpanzersprites/`, thumbnails)

## Useful Entry Points for Agents

1. Start reading `lib/flexlay.hpp` and `lib/workspace.hpp` for the core API.
2. Look at `paint/paint.rb` or `netpanzer/netpanzer.rb` for a working front-end structure.
3. GUI layout and keybindings are defined on the Ruby side (`gui.rb`, layout S-exprs, `data/gui/gui.xml`).
4. Tile / object selection and painting logic lives under `lib/tools/` and `lib/gui/`.
5. When fixing SWIG/Ruby issues, start from the patterns documented above in `ruby/flexlay.rb`, `lib/flexlay_wrap.i`, and `lib/clanlib.i`.

---

*This file is intended as a quick orientation document for automated agents and new contributors.*
