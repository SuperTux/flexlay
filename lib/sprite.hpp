// Flexlay - A Generic 2D Game Editor
// Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
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

#ifndef HEADER_SPRITE_HPP
#define HEADER_SPRITE_HPP

#include <ClanLib/Display/sprite.h>
#include <ClanLib/Display/sprite_description.h>

#include "color.hpp"
#include "surface.hpp"

class Sprite
{
private:
  CL_Sprite m_sprite;

public:
  Sprite()
  {}

  Sprite(CL_SpriteDescription desc) :
    m_sprite(desc)
  {
  }

  Sprite(const std::string& name, CL_ResourceManager* resources) :
    m_sprite(name, resources)
  {}

  void draw(float x, float y, CL_GraphicContext* gc = 0)
  {
    m_sprite.draw(x, y, gc);
  }

  int get_width() const
  {
    return m_sprite.get_width();
  }

  int get_height() const
  {
    return m_sprite.get_height();
  }

  void set_scale(float x, float y)
  {
    m_sprite.set_scale(x, y);
  }

  void set_blend_func(CL_BlendFunc src, CL_BlendFunc dest)
  {
    m_sprite.set_blend_func(src, dest);
  }

  void set_blend_func_separate(CL_BlendFunc src, CL_BlendFunc dest,
                               CL_BlendFunc src_alpha, CL_BlendFunc dest_alpha)
  {
    m_sprite.set_blend_func_separate(src, dest, src_alpha, dest_alpha);
  }

  void set_color(const Color& color)
  {
    m_sprite.set_color(color.to_cl());
  }

  void set_alpha(float alpha)
  {
    m_sprite.set_alpha(alpha);
  }

  void set_alignment(CL_Origin origin, int x = 0, int y = 0)
  {
    m_sprite.set_alignment(origin, x, y);
  }

  void set_angle(float angle)
  {
    m_sprite.set_angle(angle);
  }

  void get_alignment(CL_Origin& origin, int& x, int& y) const
  {
    m_sprite.get_alignment(origin, x, y);
  }

  void get_scale(float& x, float& y) const
  {
    m_sprite.get_scale(x, y);
  }

  void add_frame(Surface surface, const Rect& rect = Rect(0,0,0,0))
  {
    m_sprite.add_frame(surface.to_cl(), rect.to_cl());
  }

  explicit operator bool() const
  {
    return m_sprite;
  }

  CL_Sprite to_cl() { return m_sprite; }
};

#endif

/* EOF */
