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

#include "tile_brush.hpp"

#include "math/rect.hpp"

TileBrush::TileBrush()
{
  opaque = false;
}

TileBrush::TileBrush(const Field<int>& f, int w, int h, int pos_x, int pos_y)
  : data(f, w, h, pos_x, pos_y)
{
  opaque = false;
}

TileBrush::TileBrush(int w, int h)
  : data(w, h)
{
  opaque = false;
}

void
TileBrush::auto_crop()
{
  Rect rect(Point(0, 0), Size(0, 0));

  for(int y = 0; y < get_height(); ++y)
    for(int x = 0; x < get_width(); ++x)
      if (at(x, y) != 0)
      {
        rect.top = y;
        goto bottom;
      }

bottom:
  for(int y = get_height()-1; y >= 0; --y)
    for(int x = 0; x < get_width(); ++x)
      if (at(x, y) != 0)
      {
        rect.bottom = y + 1;
        goto left;
      }

left:
  for(int x = 0; x < get_width(); ++x)
    for(int y = 0; y < get_height(); ++y)
      if (at(x, y) != 0)
      {
        rect.left = x;
        goto right;
      }

right:
  for(int x = get_width() - 1; x >= 0; --x)
    for(int y = 0; y < get_height(); ++y)
      if (at(x, y) != 0)
      {
        rect.right = x + 1;
        goto end;
      }

end:
  if (rect.get_width() != 0)
    resize(rect.get_width(), rect.get_height(),
           -rect.left, -rect.top);
  else
  {
    (*this) = TileBrush(1, 1);
    at(0, 0) = 0;
    set_opaque();
  }
}

void
TileBrush::set_data(const std::vector<int>& data_)
{
  return data.set_data(data_);
}

std::vector<int>
TileBrush::get_data()
{
  return data.get_data();
}

/* EOF */
