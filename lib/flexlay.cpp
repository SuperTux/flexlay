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

#include "flexlay.hpp"

#include <ClanLib/core.h>
#include <ClanLib/display.h>
#include <ClanLib/gui.h>
#include <ClanLib/gl.h>
#include "globals.hpp"
#include "fonts.hpp"

Flexlay* Flexlay::current_ = 0;

Flexlay::Flexlay()
  : window(0),
    initialized(false)
{
  screen_width  = 800;
  screen_height = 600;
  fullscreen    = false;
  allow_resize  = false;
  use_opengl    = true;

  current_ = this;
}

CL_Signal_v2<int, int>&
Flexlay::sig_resize()
{
  static CL_Signal_v2<int, int> dummy;
  if (!window)
    return dummy;
  return window->sig_resize();
}

void
Flexlay::init(const std::string& title, int width, int height, bool fullscreen_, bool allow_resize_)
{
  if (initialized)
    return;

  screen_width  = width;
  screen_height = height;
  fullscreen    = fullscreen_;
  allow_resize  = allow_resize_;

  std::cout << "Flexlay::init()" << std::endl;
  try {
#ifdef WIN32
    CL_SetupCore::set_instance(GetModuleHandle("flexlay_wrap.dll"));
#endif
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
    CL_SetupGUI::init();

    window = new CL_DisplayWindow(title,
                                  screen_width, screen_height, fullscreen, allow_resize);

    resources = CL_ResourceManager(datadir + "/flexlay.xml");
    Fonts::verdana11        = CL_Font("verdana11_black", &resources);
    Fonts::verdana11_yellow = CL_Font("verdana11_yellow", &resources);
    initialized = true;
  } catch (CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
  }
}

void
Flexlay::deinit()
{
  if (!initialized)
    return;

  std::cout << "Flexlay::deinit()" << std::endl;

  // Destroy the display window before tearing down subsystems so
  // ClanLib GUI/GL do not run finalizers against a dead display.
  delete window;
  window = 0;

  try {
    CL_SetupGUI::deinit();
  } catch (...) {
    std::cout << "Flexlay::deinit: CL_SetupGUI::deinit failed" << std::endl;
  }

  try {
    CL_SetupDisplay::deinit();
  } catch (...) {
    std::cout << "Flexlay::deinit: CL_SetupDisplay::deinit failed" << std::endl;
  }

#ifdef HAVE_LIBSDL
  try {
    if (use_opengl)
      CL_SetupGL::deinit();
    else
      CL_SetupSDL::deinit();
  } catch (...) {
    std::cout << "Flexlay::deinit: GL/SDL deinit failed" << std::endl;
  }
#else
  try {
    CL_SetupGL::deinit();
  } catch (...) {
    std::cout << "Flexlay::deinit: CL_SetupGL::deinit failed" << std::endl;
  }
#endif

  try {
    CL_SetupCore::deinit();
  } catch (...) {
    std::cout << "Flexlay::deinit: CL_SetupCore::deinit failed" << std::endl;
  }

  initialized = false;
}

void
Flexlay::set_datadir(const std::string& datadir_)
{
  datadir = datadir_;
}

/* EOF */
