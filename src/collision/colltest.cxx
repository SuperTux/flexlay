//  $Id: colltest.cxx,v 1.6 2003/11/13 12:59:42 grumbel Exp $
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

#include "collision_mask.hxx"

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

        CollisionMask mask1("../data/images/colltest2.png");
        CollisionMask mask2("../data/images/colltest.png");

        CL_Surface sprite1("../data/images/colltest2.png");
        CL_Surface sprite2("../data/images/colltest.png");

        float scale_x = 1.0f;
        float scale_y = 1.0f;

        while (!CL_Keyboard::get_keycode(CL_KEY_ESCAPE))
          {
            if (CL_Mouse::get_keycode(CL_MOUSE_LEFT))
              {
                scale_x += 0.01f;
              }
            else if (CL_Mouse::get_keycode(CL_MOUSE_RIGHT))
              {
                scale_x -= 0.01f;
              }

            sprite2.set_scale(scale_x, scale_y);
        
            int mx = CL_Mouse::get_x();
            int my = CL_Mouse::get_y();

            if (mask1.collides_with(mask2, 
                                    mx - 150, my - 150,  scale_x, scale_y))
              {
                CL_Display::clear(CL_Color::black);
              }
            else
              {
                CL_Display::clear(CL_Color(255, 255, 0));
              }

#if 0
            if (mask1.collides_with(mask2, 
                                    mx - 320, my - 240))
              {
                if (!mask1.slow_pixel_collides_with(mask2, 
                                                    mx - 320, my - 240))
                  std::cout << "False positiv" << std::endl;
                CL_Display::clear(CL_Color(255, 255, 255));
              }
            else
              {
                if (mask1.slow_pixel_collides_with(mask2, 
                                                   mx - 320, my - 240))
                  std::cout << "Collision not detected" << std::endl;
                CL_Display::clear(CL_Color(255, 255, 0));
              }
#endif 
            sprite1.draw(150, 150);
            sprite2.draw(mx, my);

            CL_Display::flip();
            CL_System::keep_alive();
            CL_System::sleep(10);
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
