//  $Id: tile_editor.hxx,v 1.1 2003/09/22 18:37:05 grumbel Exp $
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

#ifndef HEADER_TILE_EDITOR_HXX
#define HEADER_TILE_EDITOR_HXX

#include <ClanLib/GUI/component.h>
#include <ClanLib/Signals/slot_container.h>
#include <ClanLib/Core/Math/point.h>

class Tile;

/** */
class TileEditor : public CL_Component
{
private:
  Tile* tile;
  CL_SlotContainer slots;
  CL_Point mouse_pos;
protected:
  virtual ~TileEditor();
public:
  TileEditor(int x, int y, int w, int h, CL_Component* parent);
  
  void draw();
  void mouse_move(const CL_InputEvent& event);
  void mouse_down(const CL_InputEvent& event);
  void mouse_up  (const CL_InputEvent& event);

  void set_tile(Tile* tile);
private:
  void paint(CL_Point pos, bool val);

  TileEditor (const TileEditor&);
  TileEditor& operator= (const TileEditor&);
};

#endif

/* EOF */
