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

#ifndef HEADER_EDITOR_TILE_BRUSH_HXX
#define HEADER_EDITOR_TILE_BRUSH_HXX

#include "../field.hxx"

/** */
class TileBrush : public Field<int>
{
private:
  /** if true transparent tiles are drawn the same as opaque tiles, ie
      erasing tiles formaly on the map and replacing them. If false
      transparent tiles are not drawn at all, thus letting the old
      tiles stay in place */
  bool opaque;

public:
  TileBrush();
  TileBrush(int w, int h);

  void set_opaque() { opaque = true; }
  void set_transparent() { opaque = false; }

  bool is_opaque() const { return opaque; }
  
  /** Removes unneeded transparent bordering */
  void auto_crop();
};

#endif

/* EOF */
