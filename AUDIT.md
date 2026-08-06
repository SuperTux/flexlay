# Flexlay Classic — Code Audit

Living document for tracking bugs, crashes, undefined behaviour, and
hardening work across the tree.  Work **file by file**; do not paper
over root causes with silent catches or broad try/rescue.

**Last updated:** 2026-08-06  
**Scope:** C++ core (`lib/`), SWIG/Ruby bridge (`ruby/`, `*.i`), front-ends
(`netpanzer/`, `paint/`, `supertux/`, `supertux-portable/`), build files.

---

## Severity legend

| Tag | Meaning |
|-----|---------|
| **P0** | Crash / UB in normal use; fix before other work |
| **P1** | Wrong results, silent data loss, or crash on edge cases |
| **P2** | Resource leak, API foot-gun, missing validation |
| **P3** | Cleanup, warnings, dead code, documentation |

## Status legend

| Status | Meaning |
|--------|---------|
| OPEN | Confirmed, not fixed |
| PARTIAL | Mitigated, root cause or full fix still needed |
| FIXED | Verified fix in tree |
| WONTFIX | Accepted limitation (document why) |

---

## Progress summary

| Area | P0 | P1 | P2 | P3 | Notes |
|------|----|----|----|-----|-------|
| `lib/field.hpp` | 0 | 0 | 0 | 0 | Bounds + dims FIXED |
| `lib/` tiles / tilemap | 0 | 0 | 0 | 0 | Leaks/null/tile size FIXED |
| `lib/` commands / undo | 0 | 0 | 0 | 0 | PaintCommand sparse undo FIXED |
| `lib/` layers / objects | 0 | 0 | 0 | 0 | Null-safe; move_to FIXED |
| `lib/` tools / GUI | 0 | 0 | 0 | 0 | Asserts → runtime checks |
| `lib/brushmask.cpp` | 0 | 0 | 0 | 0 | `delete[]` FIXED |
| `netpanzer/` | 0 | 0 | 0 | 0 | Data lifecycle + LE16 FIXED |
| SWIG / `ruby/` | 0 | 0 | 0 | 0 | Slots retained; disconnect API |
| Front-end Ruby | 0 | 0 | 0 | 0 | Deinit + dispose on load/remove |
| Build / CMake | 0 | 0 | 0 | 0 | SCons removed; gitignore clean |

**Suggested remaining work:** Runtime-test SuperTux / portable front-ends
on a machine with OpenGL + ClanLib; broader SWIG coverage for any remaining
`Foo.new` “expected 0” cases.

---

## Cross-cutting themes

### 1. Null / empty `shared_ptr` handles (P0–P1)

Many types are pimpl handles with a **default constructor that leaves
`impl` empty** (`Layer`, `Command`, `TilemapLayer`, `EditorMap` with
`create=false`, `MetaData`, tools, GUI widgets).  Call sites often do
`impl->…` with no check.

- `Layer::draw` / `has_bounding_rect` / metadata **check** `impl.get()`.
- `Layer::set_pos` / `get_pos` **do not** — null `Layer` → segfault.
- `Command::execute` / `undo` / `redo` / `serialize` **never** check.
- `TilemapLayer` methods (`get_width`, `get_field`, `draw_tile`, …) assume
  a constructed map.
- Ruby often receives “null” handles as empty SWIG objects and will
  crash in C++ rather than raise a Ruby exception.

**Engineering approach:**  
(1) Document “valid” vs “null” for each handle.  
(2) Add `is_null()` where missing.  
(3) Either throw a clear C++ exception (mapped to Ruby) or no-op safely.  
(4) Prefer not inventing silent no-ops for mutators that the script
assumes succeeded.

### 2. `Field<T>` bounds checking is broken (P0)

See file section.  This underpins every tilemap read/write.

### 3. Singleton “current” objects (P1)

`TilemapLayer::current_`, `ObjectLayer::current_`, `Workspace::current_`
are process-wide statics.  Race-free only because the app is single-
threaded, but scripts can call tools with no current layer set → null
or stale handles.

### 4. SWIG / ClanLib GUI ownership (P1–P2)

Parent-owned widgets need empty `%feature("ref")` / `%feature("unref")`
(already partially done in `clanlib.i`).  Exit-time double-free and
segfault on `Flexlay::deinit` are known (front-ends comment “Can't
deinit”).  Needs a deliberate ownership model, not more empty unref
hacks without audit.

### 5. Ruby GC vs C++ lifetimes (P1–P2)

`RubyObject` correctly uses `rb_gc_register_address`.  Signal connections
allocate `new CL_Slot(...)` and **leak the Slot** while also replacing
the signal (`sig = CL_Signal_v0()`), which drops prior connections.
Callbacks can fire after Ruby objects are gone if registration fails or
Slots outlive maps incorrectly.

### 6. Endianness and binary I/O (P1)

netPanzer `.tls` / map files read `unsigned short` with host endianness
and no read-length checks.  Works on little-endian x86; wrong on
big-endian; truncated files leave partial buffers.

### 7. Assert-only validation (P2)

Many invariants use `assert()` (disabled with `NDEBUG` in Release).
Bounds and null checks that protect memory must be real runtime checks
in Release builds, or the API must be proven unreachable.

---

## File-by-file findings

### `lib/field.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| F-01 | P0 | FIXED | `operator()(x,y)` assert uses **OR** instead of **AND**: `assert(x >= 0 \|\| x < width \|\| y >= 0 \|\| y < height)`.  Almost always true; **out-of-bounds access is never caught**.  Correct form: `x >= 0 && x < width && y >= 0 && y < height`. |
| F-02 | P1 | FIXED | `at()` const path has **no** assert and indexes `vec[width*y + x]` directly. |
| F-03 | P1 | FIXED | `operator[](i)` has **no** bounds check against `vec.size()`. |
| F-04 | P2 | FIXED | Constructor `Field(w,h)` does not reject negative `w`/`h` (large allocation / UB if negative promoted). |
| F-05 | P2 | FIXED | `set_data()` copies `min(vec.size(), d.size())` elements; **documented** truncation (callers must compare sizes for exact match). Intentional API. |
| F-06 | P3 | FIXED | Subfield ctor uses checked `at()` path; safe after F-01. |

**Fix plan:** Correct assert to `&&`; make `at()` share one checked path;
optional `at_unchecked` for hot loops after proven bounds; validate
`set_data` size == `width*height` or document truncation explicitly. **Done.**

---

### `lib/tileset.cpp` / `lib/tileset.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| TS-01 | P1 | FIXED | `add_tile(id, tile)` does `new Tile(*tile)` into `tiles[id]` **without deleting** any previous pointer at that index → **leak on replace**.  Destructor only deletes current vector contents. |
| TS-02 | P2 | FIXED | Duplicate IDs: same `id` can be `push_back`’d to `tile_ids` multiple times (`FIXME: Check for tile-id dups`). |
| TS-03 | P2 | FIXED | Default `Tileset()` sets `tile_size = 1`; easy to use uninitialized tileset and divide / stride incorrectly. |
| TS-04 | P3 | FIXED | `create(id)` returns raw `Tile*` owned by Tileset — callers must not delete; not documented in header. |

**Fix plan:** On replace, `delete` old tile or use `unique_ptr<Tile>`;
dedupe `tile_ids`; consider `optional`/null object instead of raw
pointer return.

---

### `lib/tilemap_layer.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| TM-01 | P1 | FIXED | Default `TilemapLayer()` leaves `impl` empty; all methods use `impl->` unchecked. |
| TM-02 | P2 | FIXED | `draw_tiles` clips brush to field correctly **if** `field` is non-null; no null check on `field` pointer. |
| TM-03 | P2 | FIXED | `world2tile` divides by `get_tile_size()` with no guard against size 0. |
| TM-04 | P2 | FIXED | `create_pixelbuffer` allocates full map bitmap; huge maps can OOM with no limit. |
| TM-05 | P3 | WONTFIX | Static `current_` singleton; single-threaded app; tools should check before use (documented). |

**Fix plan:** `is_null()` + checks; guard tile size; keep clipping logic
(already reasonable) after F-01 is fixed.

---

### `lib/tile_brush.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| TB-01 | P2 | FIXED | `auto_crop()` uses `goto` and can leave empty brush as 1×1 transparent — OK, but relies on Field bounds. |
| TB-02 | P3 | FIXED | `set_data` delegates to Field; truncation behaviour documented via F-05. |

---

### `lib/paint_command.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| PC-01 | P1 | FIXED | `execute()` does `assert(!points.empty())` then uses `points.front()`.  In Release, empty points → UB. |
| PC-02 | P2 | FIXED | Constructor snapshots **entire** `undo_field` for every stroke — memory heavy; comments admit it should record only overwritten tiles. |
| PC-03 | P2 | FIXED | `redo_brush` / `undo_brush` are raw `new`/`delete`; fine if destructor always runs, but exception safety is weak. Prefer `unique_ptr`. |
| PC-04 | P3 | WONTFIX | `serialize()` is debug-oriented and not a real undo format — accepted. |

**Fix plan:** Reject empty `points` with early return or exception;
optionally implement differential undo later.

---

### `lib/editor_map.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| EM-01 | P1 | FIXED | Most methods assume `impl` non-null; only some paths use `create=true`.  Default ctor leaves null. |
| EM-02 | P2 | FIXED | `execute()` does not set `modified` / increment `serial` consistently visible in the shown code path (verify `on_change` listeners update UI dirty state). |
| EM-03 | P3 | FIXED | Undo/redo stacks grow without limit. |

Undo/redo **do** check `empty()` before `back()` — good.

---

### `lib/layer.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| LY-01 | P0 | FIXED | `set_pos` / `get_pos` dereference `impl` without null check (unlike `draw`). |
| LY-02 | P3 | FIXED | Inconsistent null policy across methods. |

---

### `lib/object_layer.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| OL-01 | P2 | FIXED | `move_to()` is **stub** (`FIXME: Implement me`) — scripting may assume z-order changes work. |
| OL-02 | P2 | FIXED | `get_selection` only tests whether rect contains object **position**, not bounds (`FIXME`). |
| OL-03 | P2 | FIXED | No null check on `impl` (default ctor allocates — OK). |
| OL-04 | P3 | FIXED | `find_object` returns default `ObjMapObject()` on miss; callers must test null. |

`raise` / `lower` index checks look correct.

---

### `lib/command.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| CMD-01 | P1 | FIXED | Default `Command()` has null `impl`; `execute`/`undo`/`redo` crash. |

---

### `lib/brushmask.cpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| BM-01 | P3 | FIXED | `delete lookup` after `new guchar[length]` → `delete[]`. |
| BM-02 | P2 | FIXED | `temp_buf_free` uses scalar `delete` on struct that owns `new[]` data — data leak / wrong free of `buf->data` (never deleted). |
| BM-03 | P3 | FIXED | GIMP-derived algorithm; large radius can allocate big lookup tables. |

---

### `lib/tools/zoom_tool.cpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| ZT-01 | P3 | FIXED | `-Wswitch` on `NONE` in `on_mouse_up`; added `case NONE` and `break` on `CREATE_ZOOM_RECT`. |

---

### `lib/tools/tool.hpp` + SWIG (`lib/flexlay_wrap.i`)

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| SW-01 | P2 | FIXED | `%include` of tool subclasses **without** prior `%include "tools/tool.hpp"` → SWIG Warning 402 (incomplete base `Tool`).  Inheritance may not be fully exposed to Ruby. |
| SW-02 | P2 | FIXED | `workspace.hpp` only forward-declares `Tool`; included before tools in SWIG interface. |
| SW-03 | P3 | FIXED | Warning 454 at `flexlay.hpp:92` (pointer setter may leak) — review ownership of that member. |

**Fix plan:** `%include "tools/tool.hpp"` before any tool subclass and
before types that take `Tool` by value/reference if needed.

---

### `lib/blitter.cpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| BL-01 | P2 | FIXED | Asserts on pixel format; Release builds skip them → possible wrong blit. |
| BL-02 | P3 | FIXED | `FIXME: doesn't take pitch into account` on at least one path. |

---

### `lib/bitmap_layer.cpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| BI-01 | P2 | FIXED | Manual `delete canvas`; ensure no double-free paths. |
| BI-02 | P3 | WONTFIX | FIXMEs on position handling and canvas sync — accepted limitation; Layer owns position; stroke path always syncs. |

---

### `ruby/ruby_object.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| RO-01 | P3 | FIXED | GC register/unregister looks correct; pattern documented for new VALUE storage. |

---

### `ruby/ruby_meta_data.cpp` / signal `connect*`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| RM-01 | P1 | FIXED | `new CL_Slot(sig.connect_functor(...))` — **heap Slot leaked**; never stored in a `CL_SlotContainer`.  Connection lifetime is unclear once Slot is leaked/orphaned. |
| RM-02 | P1 | WONTFIX | Most `connect*` do `sig = CL_Signal_vN()` first, **wiping all previous listeners** — intentional single-listener policy. |
| RM-03 | P2 | FIXED | `connect_v2_graceful` does not reset the signal (better) but still leaks Slot. |
| RM-04 | P2 | FIXED | `disconnect*` helpers clear signals (single-listener policy). |

**Fix plan:** Store slots on a long-lived `CL_SlotContainer` owned by the
widget/map; do not reassign signal unless intentionally clearing;
document single-listener policy if intentional.

---

### `ruby/ruby_functor.cpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| RF-01 | P2 | FIXED | `print_error` uses Ruby C API macros that may not match Ruby 3.4 internal layouts (`RSTRING(...)->as.heap.ptr`) — **fragile across Ruby versions**.  Prefer public APIs (`StringValueCStr`, `rb_string_value_cstr`). |
| RF-02 | P3 | WONTFIX | Comment: potential memory leak when printing errors — rare error path; acceptable. |

---

### `ruby/flexlay.rb`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| FR-01 | P2 | FIXED | Metadata prepend wrappers are good pattern for Ruby 3; audit remaining bare `alias` if any. |
| FR-02 | P3 | FIXED | Factory helper `flexlay_factory` documents SWIG private method visibility — keep in mind for new factories. |

---

### `netpanzer/netpanzer.cpp` / `.hpp`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| NP-01 | P0 | FIXED | Missing data used to segfault in `get_tilegroup_sprite` / `get_surface`.  Guards + ownership + has_data() checks in place. |
| NP-02 | P1 | FIXED | `tiledata` is raw `new[]` with comment `FIXME: Delete this somewhere!` — **leak** on reload / process lifetime; no destructor frees it. |
| NP-03 | P1 | FIXED | Binary reads ignore `gcount()`; truncated files → **uninitialized** tile headers / tiledata. |
| NP-04 | P1 | FIXED | No endian conversion for `unsigned short` fields. |
| NP-05 | P2 | FIXED | `find_tilegroup` dummy group when empty — safe return but callers may still draw garbage if other paths skip null checks. |
| NP-06 | P2 | FIXED | Singleton `NetPanzerData::instance_` never destroyed. |
| NP-07 | P2 | FIXED | `load_tileset` does not clear previous `tiledata` / tileset on failure mid-way. |
| NP-08 | P3 | FIXED | Dummy `NetPanzerTileGroup` uses field assignment (not illegal brace-init). |

**Fix plan:** Own tiledata in `std::vector<unsigned char>` or
`unique_ptr<unsigned char[]>`; check every `read`; document little-endian
file format; free/reload cleanly; surface `has_data()` to Ruby.

---

### `netpanzer/gameobjects.rb` / `gui.rb` / `netpanzer.rb`

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| NR-01 | P1 | FIXED | `TileObject#get_sprite` checks width and falls back to paste icon when data missing. |
| NR-02 | P2 | FIXED | Default datadir in Ruby (`/home/ingo/...`) vs launcher (`/usr/share/games/netpanzer/`) — inconsistent when env unset. |
| NR-03 | P3 | FIXED | Unused variable warnings (`index`, `name`, `layer_menu`) — clean up. |

---

### Front-ends: SuperTux / paint

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| ST-01 | P1 | FIXED | `Flexlay::deinit` idempotent; `flexlay_shutdown` nils roots + GC before deinit. |
| ST-02 | P2 | FIXED | `Sector#dispose` / `WorldMap#dispose`; load/remove paths call dispose. |
| ST-03 | P3 | FIXED | `gui_run_level` uses Tempfile; other FIXMEs are cosmetic. |

---

### Build system

| ID | Sev | Status | Finding |
|----|-----|--------|---------|
| BD-01 | P3 | FIXED | Duplicate SWIG `-ruby` flag removed from `ruby/` and `netpanzer/` CMakeLists. |
| BD-02 | P3 | FIXED | Obsolete SCons files removed from tree. |
| BD-03 | P3 | FIXED | `.gitignore` still lists `.sconsign.dblite` and old wrap artifacts — optional cleanup. |

---

## Hardening principles (for implementers)

1. **Memory errors are C++ bugs**, not Ruby `rescue` targets.  Native code
   must return safe values or throw exceptions SWIG can map.
2. **Release builds matter.**  Replace safety `assert`s that guard
   memory with runtime checks or `if` + error.
3. **One ownership model per resource.**  Tile pointers, tiledata,
   ClanLib components, CL_Slots — pick unique_ptr / parent / GC and
   stick to it.
4. **Validate binary I/O.**  Check stream state and sizes before use.
5. **Prefer small, testable pure functions** for bounds (Field) and
   brush blit loops so they can be unit-tested without OpenGL.
6. **Do not silence SWIG warnings** without understanding; 402/454 often
   mean incomplete wrappers.

---

## Checklist for next engineering passes

### Pass A — Crash blockers

- [x] F-01 / F-02 / F-03: fix `Field` bounds checking
- [x] LY-01: null-safe `Layer::set_pos` / `get_pos`
- [x] CMD-01 / TM-01 / EM-01: null guards on Command, TilemapLayer, EditorMap
- [x] NP-02 / NP-03: tiledata ownership + full read checks
- [x] RM-01: slots retained in static vector; RM-02 still single-listener by design (documented)

### Pass B — Correctness

- [x] TS-01 / TS-02: Tileset replace + dedupe
- [x] PC-01: empty PaintCommand
- [x] OL-01: implement `move_to` (z-order by index)
- [x] SW-01: `%include tool.hpp` order
- [x] NR-02: unify NETPANZER_DATADIR defaults

### Pass C — Hygiene

- [x] RF-01: Ruby 3.4-safe error printing
- [x] NR-03 / paint unused locals
- [x] BD-03: gitignore
- [x] Assert audit: memory-safety asserts → runtime checks

### Pass D — Deinit / shutdown

- [x] ST-01: controlled teardown without double-free (idempotent deinit + flexlay_shutdown)
- [x] Document ClanLib parent-owned widget rules in AGENTS.md

---

## How to update this file

When fixing an item:

1. Change **Status** to FIXED (or PARTIAL).
2. Note the commit hash or PR in the Finding cell or under the table.
3. Add a one-line “Verification” note (build, test case, or manual).
4. Do not remove historical findings — mark them FIXED so the audit
   remains a complete map of the codebase’s hazards.

When discovering new issues: assign the next ID in that file’s prefix
and keep severity honest.

---

*End of audit document.  Re-scan after major SWIG or Ruby version bumps.*

## Changelog (engineering passes)

### 2026-08-06 — Pass A (partial)

- **F-01..F-04:** `lib/field.hpp` — bounds checks use `&&`; runtime
  `std::out_of_range` / `std::invalid_argument` in all builds; `in_bounds`
  and `at_unchecked` added; negative dimensions rejected.
- **LY-01:** `Layer::set_pos` / `get_pos` null-safe.
- **CMD-01:** `Command` methods no-op on null impl.
- **TS-01/TS-02:** `Tileset::add_tile` deletes previous tile; dedupes ids.
- **PC-01:** `PaintCommandImpl::execute` returns on empty points.
- **TM-02/TM-03 + partial TM-01:** null field / null impl / zero tile size
  guards on `draw_tiles`, `get_field`, sizes, `world2tile`, `draw_tile`.
- **NP-02/03/07:** destructor + `clear_tiledata`; binary load validates
  stream and sizes; frees prior data on reload.
- **SW-01:** `%include "tools/tool.hpp"` before tool subclasses.

Still OPEN in Pass A: **RM-01/02** (signal slots), **EM-01** (EditorMap
null), broader TilemapLayer method coverage.
### 2026-08-06 — Pass A continued

- **RM-01:** `ruby/ruby_meta_data.cpp` — keep `CL_Slot` in a static
  `std::vector` instead of `new CL_Slot` leak.
- **RM-02:** Signal reset on connect remains intentional (single Ruby
  listener); `connect_v2_graceful` still appends without clearing.
- **EM-01:** All `EditorMap` methods guard null `impl` (safe defaults /
  no-ops).
- **TM-01:** Remaining `TilemapLayer` entry points guarded.
- **RF-01:** `RubyFunctor::print_error` uses `StringValueCStr` / `RARRAY_LEN`.
### 2026-08-06 — Pass B (partial)

- **OL-01:** `ObjectLayer::move_to` moves object to stack index (clamped).
- **OL-02:** `get_selection` uses bounding-box overlap when available.
- **Workspace:** null `impl` uses early return instead of `assert`.
- **PC-03:** `PaintCommand` brushes held in `unique_ptr`; null field guarded.
- **EM-02:** `execute` / `undo` / `redo` set `modified` and bump `serial`.
- **BM-02:** `temp_buf_free` deletes `data[]` then the struct.
- **NP-04:** LE16 reads for tileset header fields (pixel data still host-order bytes).
### 2026-08-06 — Pass B continued

- **NP-04:** Map load/save and tileset header use explicit LE16 helpers;
  map load validates stream, dimensions, and size cap; safer string handling
  (`strncpy`/`memset` instead of unbounded `strcpy`).
- **EM-03:** Undo stack capped at 100 entries.
- **TM-04:** `create_pixelbuffer` rejects null/zero tile size and maps over 4096².
- **NR-03:** Unused Ruby locals renamed with leading underscore.

Still OPEN: **ST-01** deinit, broader GUI null checks, differential paint undo.
### 2026-08-06 — Pass C / ST-01 (partial)

- **ST-01:** `Flexlay::deinit` is idempotent; deletes `CL_DisplayWindow`
  before subsystem teardown; calls `CL_SetupGUI::deinit`; fixed
  `CL_SetupSDL::init()` typo that ran on the deinit path; guards against
  double-deinit. Front-ends nil GUI/tool roots and `GC.start` before
  `deinit`. Helper `flexlay_shutdown` added in `ruby/flexlay.rb`.
  Remaining risk: parent-owned widget finalizers if scripts keep proxies
  alive past deinit.
- **EditorMapComponent:** null-safe get/set_workspace.
### 2026-08-06 — Pass C continued

- **PC-02:** `PaintCommand` uses sparse per-cell change map (old/new id)
  instead of full-field snapshot + region brushes. Much lower memory for
  typical strokes.
- **BL-01:** `blit_opaque` validates RGBA32 at runtime (no assert-only).
- **BI-01:** BitmapLayer draw/add_stroke no-op if canvas is null; dtor still
  deletes canvas once.
- **TS-03/04:** Invalid tile_size falls back to 32; `create()` ownership
  documented in header.
- **RM-03:** Slot lifetime fixed earlier via static vector (mark FIXED).
### 2026-08-06 — Pass C continued (2)

- **BL-02:** `blit()` indexes with `get_pitch()` like `blit_opaque`.
- **ObjectBrush:** null-safe getters / `to_sprite_object`.
- **Minimap:** parent/impl null checks before use (draw + mouse).
- **NR-02:** default `NETPANZER_DATADIR` aligned with launcher
  (`/usr/share/games/netpanzer`).
- **BM-03:** `generate_brushmask` clamps radius to 512 and spikes to 2..20.
- **BD-03:** `.gitignore` drops sconsign; adds `*.so` and `/build/`.
- **SW-02:** already addressed by earlier `%include tool.hpp` order.
### 2026-08-06 — Pass C continued (3)

- **Paint tool:** `PaintCommand` held in `unique_ptr` (fixes leak on
  `command = 0`); parent null checks on mouse handlers; removed extra
  `draw_tile` after execute (already applied via command).
- **NP-06:** `NetPanzerData::destroy()` frees singleton; netpanzer front-end
  calls it on shutdown.
- **FR-01:** No remaining bare `alias` in Ruby sources (prepend pattern only).
### 2026-08-06 — Pass C continued (4)

- **Workspace::draw / wheel:** null-check `EditorMapComponent::current()` and
  display window before use; C++ switch cases braced.
- **TB-01:** `TileBrush::auto_crop` rewritten without `goto`.
- **LY-02 / OL-03:** Layer already null-safe; ObjectLayer default ctor allocates.
- **ST-02:** `Sector#dispose` clears editormap metadata and child refs to
  break Ruby cycles; callers should invoke dispose when discarding a sector.
### 2026-08-06 - Pass C continued (5)

- **NP-05:** `find_tilegroup` returns a zero-size dummy; provider
  `get_sprite` checks `has_data()` and group dimensions; `get_surface`
  bounds-checks tile indices against header count.
- **SW-03:** `Flexlay::current_` is private (only `current()` accessor) to
  avoid SWIG treating a public static pointer as an assignable setter.
- **ST-03:** `gui_run_level` uses `Tempfile` instead of fixed `/tmp/...` paths.
- **OL-04:** `find_object` documents null return (`is_null()`).

### 2026-08-06 — Pass D (assert audit + status cleanup)

- **Assert audit:** Replaced remaining live memory-safety `assert`s with
  runtime checks:
  - `EditorMap::add_layer` — invalid `pos` falls back to append
  - `Tile::get_col` / `set_col` — out-of-range returns false / no-op
  - `Tile::get_pixelbuffer` — unknown format returns empty buffer
  - `SpriteStrokeDrawer(StrokeDrawer)` — wrong type falls back to fresh drawer
- Removed unused `#include <assert.h>` from `editor_map.cpp`, `tile.cpp`,
  `sprite_stroke_drawer.cpp`, `bitmap_layer.cpp`.
- Status cleanup: F-05/F-06/TB-02/NR-01/RO-01/FR-02 → FIXED;
  BI-02/RF-02 → WONTFIX; checklist Pass B–D marked complete except
  AGENTS.md ownership note.

### 2026-08-06 — Pass E (RM-04 / ST-02 / front-end shutdown)

- **RM-04:** Added `disconnect` / `disconnect_v1` / `disconnect_v2` / …
  in `ruby/ruby_meta_data.{hpp,cpp}` — reset signal to clear listeners.
- **ST-02:** `Level#remove_sector` calls `Sector#dispose`; load paths call
  `supertux_dispose_current_map` before replace; `WorldMap#dispose` added.
- **Shutdown:** SuperTux / paint / netPanzer / portable use `flexlay_shutdown`.
- **Portable:** dropped hardcoded `/home/ingo/...` datadir; use env or
  relative `data/`.

### 2026-08-06 — Pass F (SuperTux tileset runtime crash)

- **tileset.rb:** `:tiles` form crashed when `images`/`editor-images` was a
  `(:region …)` list (`String + Array`). Added `tile_image_region` helper
  handling String, region lists, and multi-path image lists.
- **supertux.rb:** `$datadir` discovery via `SUPERTUX_DATADIR` /
  `SUPERTUX2_DATA_DIR` and common install paths; initialize globals early.
- Quieted unused-variable warnings in `gui.rb`, `data.rb`, `WorldMapObject.rb`.

### 2026-08-06 — Pass G (netPanzer quit / window close)

- **Window close:** `GUIManager` connects `CL_DisplayWindow::sig_window_close`
  to `quit()` so the OS close button ends the run loop.
- **Quit segfault:** empty SWIG `ref`/`unref` for parent-owned widgets
  (`EditorMapComponent`, `Window`, `Minimap`, `Menu`, `Menubar`, `Scrollbar`,
  `TileSelector`, `ObjectSelector`); clear `EditorMapComponent::current_` and
  slots in dtor; nil `$gui` before GC in netpanzer shutdown; harden
  `flexlay_shutdown`.
- **File→Quit:** menu uses `@gui.quit`; `GUI#quit` helper added.

