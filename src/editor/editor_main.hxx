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

#ifndef HEADER_EDITOR_MAIN_HXX
#define HEADER_EDITOR_MAIN_HXX

#include <ClanLib/application.h>
#include <ClanLib/Display/display.h>

/** */
class EditorMain : public CL_ClanApplication
{
private:
  int  screen_width;
  int  screen_height;
  bool fullscreen;
  bool allow_resize;
  CL_DisplayWindow* window;

public:
  EditorMain();
  ~EditorMain();

  int main(int argc, char** argv);
} app;

#endif

/* EOF */
