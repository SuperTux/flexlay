// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2002 Ingo Ruhnke <grumbel@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "editor_map.hpp"
#include "gui/editor_map_component.hpp"
#include "gui/generic_dialog.hpp"
#include "flexlay.hpp"
#include "gui_manager.hpp"
#include "math/rect.hpp"
#include "tilemap_layer.hpp"
#include "tileset.hpp"
#include "workspace.hpp"

int main()
{
  Flexlay flexlay;
  flexlay.init();

  GUIManager gui;

  EditorMap m(true);
  Tileset tileset(32);
  TilemapLayer tilemap(tileset, 20, 10);

  m.add_layer(tilemap.to_layer());

  TilemapLayer::set_current(tilemap);

  EditorMapComponent* editor_map = gui.create_editor_map_component();
  Workspace workspace(true);
  editor_map->set_workspace(workspace);
  workspace.set_map(m);

  GenericDialog* dialog = gui.create_generic_dialog("Generic Dialog");
  dialog->add_int("An Int:", 5);

  gui.run();

  flexlay.deinit();
}

/* EOF */
