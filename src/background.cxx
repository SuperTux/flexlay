//  $Id: background.cxx,v 1.4 2003/10/10 21:06:22 grumbel Exp $
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

#include <ClanLib/gl.h>
#include <ClanLib/Display/display.h>
#include "display.hxx"
#include "background.hxx"

void
Background::draw()
{
  CL_Display::fill_rect(CL_Rect(0, 0, 800, 300),
                        CL_Gradient(CL_Color(  0,   0,  50),
                                    CL_Color(  0,   0,  50),
                                    CL_Color( 50,  50, 128),
                                    CL_Color( 50,  50, 128)));
  CL_Display::fill_rect(CL_Rect(0, 300, 800, 600),
                        CL_Gradient(CL_Color( 50,  50, 128),
                                    CL_Color( 50,  50, 128),
                                    CL_Color(  0,   0,   0),
                                    CL_Color(  0,   0,   0)));
}

/* EOF */
