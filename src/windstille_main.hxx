//  $Id: windstille_main.hxx,v 1.3 2003/11/04 22:48:51 grumbel Exp $
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
  CL_ConsoleWindow* console_window;

public:
  virtual char* get_title() { return "Windstille"; }
  virtual int main(int argc, char** argv);
  virtual int inner_main(void* closure, int argc, char** argv);
} main_app;

// Wrapper to call the member func
void inner_main (void* closure, int argc, char* argv[])
{
  main_app.inner_main (closure, argc, argv);
}

#endif

/* EOF */
