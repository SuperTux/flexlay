//  $Id: water_map.hxx,v 1.1 2003/09/12 16:31:21 grumbel Exp $
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

#ifndef HEADER_WATER_MAP_HXX
#define HEADER_WATER_MAP_HXX

/** */
class WaterMap
{
private:
  struct Water {
    Water(int x, int y, int w, int h)
      : x(x), y(y), w(w), h(h)
    {
    }
    int x, y;
    int w, h;
  };  

  typedef std::vector<Water> Waters;
  Waters waters;
public:
  WaterMap();
  ~WaterMap();

  void draw();
  void update(float delta);

private:
  WaterMap (const WaterMap&);
  WaterMap& operator= (const WaterMap&);
};

#endif

/* EOF */
