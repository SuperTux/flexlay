//  $Id: tilemap_object_tool.hxx,v 1.1 2003/09/23 22:10:40 grumbel Exp $
// 
//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

#ifndef HEADER_TILEMAP_OBJECT_TOOL_HXX
#define HEADER_TILEMAP_OBJECT_TOOL_HXX

#include "tilemap_tool.hxx"
#include "editor_objmap.hxx"
#include "object_brush.hxx"

class CL_Menu;
class EditorMap;

/** */
class TileMapObjectTool : public TileMapTool
{
public:
  typedef std::vector<EditorObjMap::Obj*> Selection; 

private:
  CL_Signal_v1<CL_Menu*> on_popup_menu_display;

  EditorObjMap::Obj* obj;
  enum { DRAG, SELECT, NONE } state;

  /** the position on which the object was clicked, relative to the
      object */
  CL_Point offset;

  CL_Point drag_start;
  CL_Rect selection_rect;

  Selection selection;

public:
  TileMapObjectTool();
  ~TileMapObjectTool();

  void draw();

  void on_mouse_up  (const CL_InputEvent& event);
  void on_mouse_down(const CL_InputEvent& event);
  void on_mouse_move(const CL_InputEvent& event);

  void clear_selection() { selection.clear(); }
  Selection get_selection() const { return selection; }

  CL_Signal_v1<CL_Menu*>& sig_on_popup_menu_display() { return on_popup_menu_display; }
private:
  TileMapObjectTool (const TileMapObjectTool&);
  TileMapObjectTool& operator= (const TileMapObjectTool&);
};

#endif

/* EOF */
