//  $Id: collision_benchmark.cxx,v 1.1 2003/09/02 13:52:04 grumbel Exp $
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
        CL_SetupDisplay::init(true);

        CollisionMask mask1("../../data/images/colltest2.png");
        CollisionMask mask2("../../data/images/colltest.png");
        
        int collisions = 0;
        int misses     = 0;
        int t = CL_System::get_time();
        int maxi = 1000000;

        srand(0);
        for (int i = 0; i < maxi; ++i)
          {
            if (mask1.collides_with(mask2,
                                    rand() % 100 - 50,
                                    rand() % 100 - 50))
              ++collisions;
            else
              ++misses;
          }
        
        std::cout << "Needed: " << (float)(CL_System::get_time() - t)/maxi << std::endl;
        std::cout << "Collisions: " << collisions << std::endl;
        std::cout << "Misses:     " << misses << std::endl;

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
