//  $Id: display.hxx,v 1.1 2003/08/12 08:24:41 grumbel Exp $
// 
//  Windstille - A Jump'n Shoot Game
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

#ifndef HEADER_DISPLAY_HXX
#define HEADER_DISPLAY_HXX

/** */
class Display
{
private:
public:
  /** Enter 2d OpenGL mode and allow to draw polygons and such */
  static void begin_gl();

  /** Leave 2d OpenGL mode and give controll back to ClanLib */
  static void end_gl();
};

#endif

/* EOF */
