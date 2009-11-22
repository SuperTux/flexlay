//  Pingus - A free Lemmings clone
//  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//  
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//  
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#ifndef HEADER_FLEXLAY_SPRITE_STROKE_DRAWER_HPP
#define HEADER_FLEXLAY_SPRITE_STROKE_DRAWER_HPP

#include "stroke_drawer.hpp"

class Stroke;
class SpriteStrokeDrawerImpl;

/** */
class SpriteStrokeDrawer
{
public:
  enum DrawMode { DM_NORMAL, DM_ERASE, DM_ADDITION, DM_SHADER, DM_SMUDGE  };

  SpriteStrokeDrawer(StrokeDrawer drawer);
  SpriteStrokeDrawer();

  /** The modus in which the drawing affects the image (normal, erase, addition, color, etc.) */
  void set_mode(DrawMode mode);
  DrawMode get_mode();

  StrokeDrawer to_drawer();
private:
  SharedPtr<SpriteStrokeDrawerImpl> impl;
};

#endif

/* EOF */
