//  $Id: windstille_main.hxx,v 1.4 2003/11/07 13:00:39 grumbel Exp $
// 
//  Windstille - A Jump'n Shoot Game
//  Copyright (C) 2000 Ingo Ruhnke <grumbel@gmx.de>
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

#ifndef WINDSTILLEMAIN_HXX
#define WINDSTILLEMAIN_HXX

#include <ClanLib/gl.h>
#include <ClanLib/application.h>

class WindstilleMain : public CL_ClanApplication
{
private:
  int  screen_width;
  int  screen_height;
  bool fullscreen;
  bool allow_resize;
  int  joystick_id;

  bool launch_editor;
  std::string levelfile;
  
  CL_SoundOutput*   sound;
  CL_DisplayWindow* window;
public:
  WindstilleMain();
  ~WindstilleMain();

  virtual char* get_title() { return "Windstille"; }
  virtual int main(int argc, char** argv);

private:
  void parse_command_line(int argc, char** argv);
  void init_modules();
  void deinit_modules();
} main_app;

#endif

/* EOF */
