//  $Id: collision_mask.hxx,v 1.3 2003/09/01 23:43:16 grumbel Exp $
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

typedef unsigned int cm_uint32;

/** */
class CollisionMask
{
private:
  static const int int_width;
  cm_uint32* data;

  /** Width of the collision mask in cm_uint32's */
  int width;
  int height;
  int pitch;
public:
  /** Generate a full solid collision mask */
  CollisionMask(int width, int height);

  /** Generate a collision mask from a given bitmap */
  CollisionMask(int width, int height, cm_uint32* data);
  
  /** Creates a collision mask from a file */
  CollisionMask(const std::string filename);

  ~CollisionMask();

  /** Return the int representing a 32 pixel long line
   *
   *  @param x  X in tile units (pixels/32)
   *  @param y  Y in pixel units
   */
  inline cm_uint32 get_line(int x, int y) const;

  inline void put_pixel(int x, int y, bool pixel);
  inline bool get_pixel(int x, int y) const;

  inline int  get_width()  const { return width; }
  inline int  get_height() const { return height; }

  /** Checks pixel precisly if the two collision masks collide, does
      optimizations where possible */
  bool collides_with      (const CollisionMask& mask, int x_of, int y_of) const;

  /** Checks for collision of the bounding boxes of the two collision
      masks */
  bool bbox_collides_with (const CollisionMask& mask, int x_of, int y_of) const;

  /** Checks pixel precisly if the two collision masks collide, in a
      slow but safe way */
  bool slow_pixel_collides_with(const CollisionMask& mask, int x_of, int y_of) const;

private:
  /** Checks pixel precisly if the two collision masks collide */
  bool pixel_collides_with(const CollisionMask& mask, int x_of, int y_of) const;

  CollisionMask (const CollisionMask&);
  CollisionMask& operator= (const CollisionMask&);
};

#endif

/* EOF */
