//  $Id: editor.hxx,v 1.5 2003/09/12 09:25:48 grumbel Exp $
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

#ifndef HEADER_EDITOR_HXX
#define HEADER_EDITOR_HXX

#include <stack>
#include <ClanLib/gui.h>
#include "scripting.hxx"

class EditorTileMap;

/** */
class Editor
{
private:
  CL_GUIManager*   manager;
  CL_StyleManager* style;
  CL_ResourceManager* resources;
  CL_SlotContainer* slot_container;

  CL_PopupMenu* popupmenu;
  CL_MenuData*  menu_data;

  EditorTileMap* tilemap;

  std::stack<CL_Component*> components;

  static Editor* current_;
public:
  static Editor* current() { return current_; }

  Editor();
  ~Editor();

  EditorTileMap* get_editor_tilemap() { return tilemap; }

  CL_Component* get_component() { return components.top(); }

  CL_SlotContainer* get_slot_container() { return slot_container; }

  void run();
  
  void popup_menu();

  void load(const std::string& filename);

  void push_component(CL_Component* c) { components.push(c); }
  void pop_component() { components.pop(); }
private:
  Editor (const Editor&);
  Editor& operator= (const Editor&);
};

#endif

/* EOF */
