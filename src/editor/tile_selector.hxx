//  $Id: tile_selector.hxx,v 1.2 2003/09/10 13:53:11 grumbel Exp $
// 
//  Pingus - A free Lemmings clone
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef TILESELECTOR_HXX
#define TILESELECTOR_HXX

#include <ClanLib/gui.h>

class TileSelector : public CL_Component
{
private:
  CL_SlotContainer slots;
  int width;
  int height;
  int index;
  
  int offset;
  int old_offset;
  int mouse_over_tile;
  bool scrolling;
  CL_Point mouse_pos;
  float scale;
public:
  /** width and height in number of tiles */
  TileSelector(int width, int height, CL_Component* parent);

  void set_scale(float s);

  void draw();

  void mouse_move(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_up  (const CL_InputEvent& event);
};

#endif

/* EOF */
