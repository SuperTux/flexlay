//  $Id: collision_sprite_test.cxx,v 1.2 2003/11/13 12:59:42 grumbel Exp $
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

#include <ClanLib/display.h>
#include <ClanLib/core.h>
#include <ClanLib/gl.h>
#include <ClanLib/application.h>

#include "collision_sprite.cxx"
#include "collision_mask.cxx"

class CollTest : CL_ClanApplication
{
public:
  int main(int argc, char** argv)
  {
    try 
      {
        CL_SetupCore::init();
        CL_SetupDisplay::init();
        CL_SetupGL::init();

        CL_DisplayWindow window("colltest", 640, 480, false, false);

        CL_ResourceManager* resources = new CL_ResourceManager("../data/windstille.xml");

        CL_Sprite       sprite("hero/run", resources);
        //CL_SpriteDescription desc("hero/run1", resources);
        CollisionSprite col_sprite("hero/run1", resources);

        CL_Sprite       sprite2("hero/run", resources);
        CollisionSprite col_sprite2("hero/run1", resources);

        while (!CL_Keyboard::get_keycode(CL_KEY_ESCAPE))
          {
            CL_Display::clear(CL_Color(155, 155, 155));
                
            sprite.update(5.0/1000);
            sprite2.update(8.0/1000);

            int mx = CL_Mouse::get_x();
            int my = CL_Mouse::get_y();

            if (col_sprite.get_frame(sprite.get_current_frame())
                ->collides_with(*(col_sprite2.get_frame(sprite2.get_current_frame())),
                                mx - 320, my - 240))
              {
                CL_Display::clear(CL_Color(255, 255, 255));
              }
            else
              {
              }

            sprite.draw(320, 240);
            sprite2.draw(mx, my);

            CL_Display::flip();
            CL_System::keep_alive();
            CL_System::sleep(30);
          }

        CL_SetupGL::deinit();
        CL_SetupDisplay::deinit();
        CL_SetupCore::deinit();
      }
    catch (CL_Error& err)
      {
        std::cout << "CL_Error: " << err.message << std::endl;
      }
    catch (std::exception& err)
      {
        std::cout << "std::exception: " << err.what() << std::endl;
      }
    return 0;
  };
} app;

/* EOF */
