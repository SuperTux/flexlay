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

#include "sprite_brush.hpp"

#include "math/origin.hpp"

class SpriteBrushImpl : public BrushImpl
{
public:
  Sprite sprite;

  SpriteBrushImpl(const Sprite& sprite_)
    : sprite(sprite_)
  {
    sprite.set_alignment(Flexlay_origin_center, 0, 0);
  }

  virtual ~SpriteBrushImpl()
  {
  }

  Sprite get_sprite()
  {
    return sprite;
  }

  BrushImpl* clone() const
  {
    return new SpriteBrushImpl(sprite);
  }
};

SpriteBrush::SpriteBrush(const Sprite& sprite_)
  : impl(new SpriteBrushImpl(sprite_))
{
}

Brush
SpriteBrush::to_brush()
{
  return Brush(impl);
}

/* EOF */
