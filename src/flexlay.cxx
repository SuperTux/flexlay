//  $Id$
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

#include <config.h>
#include <ClanLib/core.h>
#include <ClanLib/display.h>
#include <ClanLib/gl.h>
#include "globals.hxx"
#include "flexlay.hxx"

Flexlay::Flexlay()
{
  screen_width  = 800;
  screen_height = 600;
  fullscreen    = false;
  allow_resize  = true;
  use_opengl    = true;
}

void
Flexlay::init()
{
  std::cout << "Flexlay::init()" << std::endl;

  CL_SetupCore::init();
#ifdef HAVE_LIBSDL
  if (use_opengl)
    CL_SetupGL::init();
  else
    CL_SetupSDL::init();
#else
  CL_SetupGL::init();
#endif
  CL_SetupDisplay::init();

  datadir = "../data/";

  window = new CL_DisplayWindow(PACKAGE_STRING,
                                screen_width, screen_height, fullscreen, allow_resize);

  resources = new CL_ResourceManager();
}

void
Flexlay::deinit()
{
  std::cout << "Flexlay::deinit()" << std::endl;

  CL_SetupDisplay::deinit();

#ifdef HAVE_LIBSDL
  if (use_opengl)
    CL_SetupGL::deinit();
  else
    CL_SetupSDL::init();
#else
  CL_SetupGL::deinit();
#endif

  CL_SetupCore::deinit();
}

/* EOF */
