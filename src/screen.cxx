//  $Id: screen.cxx,v 1.3 2003/10/10 21:06:22 grumbel Exp $
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

#include "delta_manager.hxx"
#include "screen.hxx"

namespace Windstille {

Screen::Screen()
{
}

void 
Screen::display()
{
  do_pause = false;
  do_quit  = false;

  on_startup();

  DeltaManager delta_manager;
  
  while (!do_quit)
    {
      draw();
      
      float delta = delta_manager.getset ();
      float step = 10/1000.0f;
      
      while (delta > step)
        {
          update(step);
          delta -= step;
        }
      // FIXME: non constant delta isn't a good idea
      update(delta);
      
      // update(0.020f);

      CL_System::keep_alive ();
      CL_System::sleep (1);
    }

  on_shutdown();
}

} // namespace Windstille

/* EOF */
