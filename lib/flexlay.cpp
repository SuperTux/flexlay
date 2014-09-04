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

#include "flexlay.hpp"

#include <ClanLib/core.h>
#include <ClanLib/display.h>
#include <ClanLib/gui.h>
#include <ClanLib/gl.h>
#include "globals.hpp"
#include "fonts.hpp"

Flexlay* Flexlay::current_ = 0;

Flexlay::Flexlay()
{
  screen_width  = 800;
  screen_height = 600;
  fullscreen    = false;
  allow_resize  = false;
  use_opengl    = true;

  current_ = this;
}

boost::signals2::signal<void (int, int)>&
Flexlay::sig_resize()
{
  return m_sig_resize;
}

void
Flexlay::init(const std::string& title, int width, int height, bool fullscreen_, bool allow_resize_)
{
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

    window->sig_resize().connect_functor([this](int w, int h){
        m_sig_resize(w, h);
        });

    resources = CL_ResourceManager(datadir + "/flexlay.xml");
    Fonts::verdana11        = CL_Font("verdana11_black", &resources);
    Fonts::verdana11_yellow = CL_Font("verdana11_yellow", &resources);
  } catch (const CL_Error& err) {
    std::cout << "CL_Error: " << err.message << std::endl;
  }
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

void
Flexlay::set_datadir(const std::string& datadir_)
{
  datadir = datadir_;
}

/* EOF */
