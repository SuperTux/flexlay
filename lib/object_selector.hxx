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

#ifndef HEADER_OBJECTSELECTOR_HXX
#define HEADER_OBJECTSELECTOR_HXX

#include <ClanLib/GUI/component.h>
#include <ClanLib/GUI/component.h>
#include "object_brush.hxx"

/** */
class ObjectSelector : public CL_Component
{
private:
  std::vector<CL_Slot> slots;

  int width;
  int height;

  int obj_width;
  int obj_height;

  int index;
  
  int offset;
  int old_offset;
  int mouse_over_tile;
  bool scrolling;
  CL_Point click_pos;
  CL_Point mouse_pos;
  float scale;

  std::vector<ObjectBrush> brushes;
  int drag_obj;

  CL_Signal_v2<ObjectBrush, CL_Point> on_drop;

protected:
  virtual ~ObjectSelector();

public:
  ObjectSelector(const CL_Rect& rect, int obj_w, int obj_h, CL_Component* parent);

  void draw();
  void add_brush(const ObjectBrush& brush);

  CL_Signal_v2<ObjectBrush, CL_Point>& sig_drop();
private:
  void mouse_move(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_up  (const CL_InputEvent& event); 
};

#endif

/* EOF */
