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

#include <memory>

#include "color.hpp"
#include "surface.hpp"

class GraphicContext;
class SpriteImpl;

class Sprite
{
public:
  Sprite();
  Sprite(const PixelBuffer& buffer);
  Sprite(const std::string& name);

  void draw(float x, float y, GraphicContext& gc);
  int get_width() const;
  int get_height() const;
  void set_scale(float x, float y);
  void set_blend_func(BlendFunc src, BlendFunc dest);
  void set_blend_func_separate(BlendFunc src, BlendFunc dest,
                               BlendFunc src_alpha, BlendFunc dest_alpha);
  void set_color(const Color& color);
  void set_alpha(float alpha);
  void set_alignment(Origin origin, int x = 0, int y = 0);
  void set_angle(float angle);
  void get_alignment(Origin& origin, int& x, int& y) const;
  void get_scale(float& x, float& y) const;
  void add_frame(Surface surface, const Rect& rect = Rect(0,0,0,0));

  explicit operator bool() const;

private:
  std::shared_ptr<SpriteImpl> m_impl;
};

#endif

/* EOF */
