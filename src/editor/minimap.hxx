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

#ifndef HEADER_MINIMAP_HXX
#define HEADER_MINIMAP_HXX

#include <ClanLib/GUI/component.h>

/** */
class Minimap : public CL_Component
{
private:
  std::vector<CL_Slot> slots;
  bool drag_active;
  
  int last_serial;
  EditorMap* editor_map;

  EditorMapComponent* parent;
  CL_Surface minimap_surface;

  void update_minimap_surface();
public:
  Minimap(EditorMapComponent* p, const CL_Point& pos, const CL_Size& size, CL_Component* parent);
  
  void draw();
  void update_minimap();

  void mouse_move(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_up  (const CL_InputEvent& event);
};

#endif

/* EOF */
