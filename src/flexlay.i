%module flexlay

%{
#include <ClanLib/Display/color.h>
#include <ClanLib/GUI/component.h>
#include <ClanLib/GUI/button.h>
#include <ClanLib/GUI/window.h>
#include <ClanLib/Core/Math/rect.h>
#include <ClanLib/Core/Math/point.h>
#include "scripting/editor.hxx"
#include "command.hxx"
#include "paint_command.hxx"
#include "object_move_command.hxx"
#include "object_add_command.hxx"
#include "scripting/editor.hxx"
#include "tile.hxx"
#include "tile_brush.hxx"
#include "editor.hxx"
#include "editor_map_layer.hxx"
#include "editor_tilemap.hxx"
#include "editor_map.hxx"
#include "workspace.hxx"
#include "tileset.hxx"
#include "editor_map_component.hxx"
#include "flexlay.hxx"
#include "globals.hxx"
#include "python_functor.hxx"
#include "gui_manager.hxx"
%}

%include "std_string.i"
%include "std_vector.i"
%include "clanlib.i"
%include "scripting/editor.hxx"
%include "command.hxx"
%include "paint_command.hxx"
%include "object_move_command.hxx"
%include "object_add_command.hxx"
%include "scripting/editor.hxx"
%include "tile.hxx"
%include "tile_brush.hxx"
%include "editor.hxx"
%include "editor_map_layer.hxx"
%include "editor_tilemap.hxx"
%include "editor_map.hxx"
%include "workspace.hxx"
%include "tileset.hxx"
%include "editor_map_component.hxx"
%include "flexlay.hxx"
%include "globals.hxx"
%include "python_functor.hxx"
%include "gui_manager.hxx"

/* EOF */
