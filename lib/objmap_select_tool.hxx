//  $Id: tilemap_object_tool.hxx,v 1.1 2003/09/23 22:10:40 grumbel Exp $
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

#ifndef HEADER_OBJMAP_SELECT_TOOL_HXX
#define HEADER_OBJMAP_SELECT_TOOL_HXX

#include "object_layer.hxx"
#include "object_brush.hxx"
#include "tool.hxx"

class CL_Menu;
class ObjMapSelectToolImpl;

/** */
class ObjMapSelectTool
{
public:
  typedef std::vector<ObjMapObject> Selection; 

  ObjMapSelectTool();
  ~ObjMapSelectTool();

  void clear_selection();
  Selection get_selection() const;
  void set_selection(const Selection& sel);

  CL_Signal_v2<int, int>& sig_on_right_click();
  CL_Signal_v1<CL_Menu*>& sig_on_popup_menu_display();

  Tool to_tool();
private:
  SharedPtr<ObjMapSelectToolImpl> impl;
};

#endif

/* EOF */
