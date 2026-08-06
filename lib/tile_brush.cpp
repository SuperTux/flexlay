//  Flexlay - A Generic 2D Game Editor
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

#include <ClanLib/Core/Math/rect.h>
#include "tile_brush.hpp"

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
  const int w = get_width();
  const int h = get_height();
  if (w <= 0 || h <= 0)
  {
    (*this) = TileBrush(1, 1);
    at(0, 0) = 0;
    set_opaque();
    return;
  }

  int top = 0;
  int bottom = h;
  int left = 0;
  int right = w;
  bool found = false;

  for (int y = 0; y < h; ++y)
  {
    for (int x = 0; x < w; ++x)
    {
      if (at(x, y) != 0)
      {
        top = y;
        found = true;
        break;
      }
    }
    if (found)
      break;
  }

  if (!found)
  {
    (*this) = TileBrush(1, 1);
    at(0, 0) = 0;
    set_opaque();
    return;
  }

  found = false;
  for (int y = h - 1; y >= 0; --y)
  {
    for (int x = 0; x < w; ++x)
    {
      if (at(x, y) != 0)
      {
        bottom = y + 1;
        found = true;
        break;
      }
    }
    if (found)
      break;
  }

  found = false;
  for (int x = 0; x < w; ++x)
  {
    for (int y = 0; y < h; ++y)
    {
      if (at(x, y) != 0)
      {
        left = x;
        found = true;
        break;
      }
    }
    if (found)
      break;
  }

  found = false;
  for (int x = w - 1; x >= 0; --x)
  {
    for (int y = 0; y < h; ++y)
    {
      if (at(x, y) != 0)
      {
        right = x + 1;
        found = true;
        break;
      }
    }
    if (found)
      break;
  }

  const int nw = right - left;
  const int nh = bottom - top;
  if (nw > 0 && nh > 0)
    resize(nw, nh, -left, -top);
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
  data.set_data(data_);
}

std::vector<int>
TileBrush::get_data()
{
  return data.get_data();
}

/* EOF */
