//  $Id: display.cxx,v 1.2 2003/10/10 21:06:22 grumbel Exp $
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

#include <ClanLib/gl.h>
#include <ClanLib/Display/display.h>
#include "display.hxx"

namespace Windstille {

void
Display::begin_gl()
{
  int viewport[4];
  glGetIntegerv(GL_VIEWPORT, viewport);

  CL_Display::begin_3d();
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluOrtho2D (0, viewport[2], viewport[3], 0);
}

void
Display::end_gl()
{
  CL_Display::end_3d();
}

} // namespace Windstille

/* EOF */
