//  $Id: collision_mask.hxx,v 1.1 2003/09/01 15:36:02 grumbel Exp $
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

#ifndef HEADER_COLLISION_MASK_HXX
#define HEADER_COLLISION_MASK_HXX

#include <ClanLib/Core/IOData/datatypes.h>

/** */
class CollisionMask
{
private:
  static const int int_width;
  cl_uint32* data;

  /** Width of the collision mask in cl_uint32's */
  int width;
  int height;
  int pitch;
public:
  /** Generate a full solid collision mask */
  CollisionMask(int width, int height);

  /** Generate a collision mask from a given bitmap */
  CollisionMask(int width, int height, cl_uint32* data);
  
  /** Creates a collision mask from a file */
  CollisionMask(const std::string filename);

  ~CollisionMask();

  /** Return the int representing a 32 pixel long line
   *
   *  @param x  X in tile units (pixels/32)
   *  @param y  Y in pixel units
   */
  inline cl_uint32 get_line(int x, int y) const;

  inline void put_pixel(int x, int y, bool pixel);
  inline bool get_pixel(int x, int y) const;

  bool collides_with      (const CollisionMask& mask2, int x_of, int y_of) const;
  bool pixel_collides_with(const CollisionMask& mask2, int x_of, int y_of) const;
  bool slow_pixel_collides_with(const CollisionMask& mask2, int x_of, int y_of) const;
  bool bbox_collides_with (const CollisionMask& mask2, int x_of, int y_of) const;

  inline int  get_width()  const { return width; }
  inline int  get_height() const { return height; }
private:
  CollisionMask (const CollisionMask&);
  CollisionMask& operator= (const CollisionMask&);
};

#endif

/* EOF */
