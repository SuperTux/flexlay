//  $Id$
// 
//  Flexlay - A Generic 2D Game Editor
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

#include <ClanLib/Core/System/sharedptr.h>
#include <ClanLib/Display/input_event.h>
#include "graphic_context_state.hxx"

class WorkspaceImpl;
class EditorMap;
class Tool;

/** */
class Workspace
{
private:
  static Workspace current_;
public:
  static void set_current(Workspace w) { current_ = w; }
  static Workspace current() { return current_; }

  Workspace();
  Workspace(int w, int h);

  void draw();

  void mouse_up  (const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_move(const CL_InputEvent& event);

  EditorMap get_map();
  void set_map(const EditorMap& m);

  GraphicContextState& get_gc_state();

  void set_tool(const Tool& tool);

  bool is_null() const { return !impl.get(); }
private:
  CL_SharedPtr<WorkspaceImpl> impl;
};

#endif

/* EOF */
