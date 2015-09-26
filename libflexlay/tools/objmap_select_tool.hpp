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

#ifndef HEADER_FLEXLAY_TOOLS_OBJMAP_SELECT_TOOL_HPP
#define HEADER_FLEXLAY_TOOLS_OBJMAP_SELECT_TOOL_HPP

#include "../object_layer.hpp"
#include "../object_brush.hpp"
#include "tool.hpp"

class CL_Menu;
class ObjMapSelectToolImpl;

class ObjMapSelectTool
{
public:
  typedef std::vector<ObjMapObject> Selection;

public:
  ObjMapSelectTool();
  ~ObjMapSelectTool();

  void clear_selection();
  Selection get_selection() const;
  void set_selection(const Selection& sel);

  boost::signals2::signal<void (int, int)>& sig_on_right_click();
  boost::signals2::signal<void (CL_Menu*)>& sig_on_popup_menu_display();

  Tool to_tool();

private:
  std::shared_ptr<ObjMapSelectToolImpl> impl;
};

#endif

/* EOF */
