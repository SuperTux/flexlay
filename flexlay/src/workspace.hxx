//  $Id$
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

#ifndef HEADER_WORKSPACE_HXX
#define HEADER_WORKSPACE_HXX

#include <ClanLib/Display/input_event.h>
#include "graphic_context_state.hxx"

class EditorMap;

class WorkspaceItem
{
public:
  CL_Point pos;
  EditorMap* editor_map;

public:
  WorkspaceItem();
  WorkspaceItem(EditorMap* m, const CL_Point& p);
};

/** */
class Workspace
{
public:
  GraphicContextState gc_state;

  typedef std::vector<WorkspaceItem*> Items;
  Items items;

  bool scrolling;
  CL_Point click_pos;

  /** Position of the center */
  CL_Pointf old_trans_offset;

  static Workspace* current_;
public:
  static void set_current(Workspace* w) { current_ = w; }
  static Workspace* current() { return current_; }

  Workspace(int w, int h);
  ~Workspace();

  void draw();

  void mouse_up  (const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);

  void add_map(EditorMap* m, const CL_Point& p);

  WorkspaceItem* get_current_item();
  EditorMap* get_current_map();
  void set_current_map(EditorMap* );
private:
  Workspace (const Workspace&);
  Workspace& operator= (const Workspace&);
};

#endif

/* EOF */
