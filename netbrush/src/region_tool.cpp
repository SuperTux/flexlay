/*            _   ___              _   
**   _ _  ___| |_| _ )_ _ _  _ _ _| |_ 
**  | ' \/ -_)  _| _ \ '_| || (_-<|   |
**  |_||_\___|\__|___/_|  \_,_/__/|_|_|
**  netBrush - Copyright (C) 2006 Ingo Ruhnke <grumbel@gmx.de>
**
**  This program is free software: you can redistribute it and/or modify
**  it under the terms of the GNU General Public License as published by
**  the Free Software Foundation, either version 3 of the License, or
**  (at your option) any later version.
**  
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**  
**  You should have received a copy of the GNU General Public License
**  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <sstream>
#include "globals.hpp"
#include "server_connection.hpp"
#include "controller.hpp"
#include "region_tool.hpp"

RegionTool::RegionTool()
  : have_region(false)
{
}

RegionTool::~RegionTool()
{

}

void
RegionTool::on_motion(const ToolMotionEvent& ev)
{
}

void
RegionTool::on_button_press(const ToolButtonEvent& ev)
{
  if (have_region)
    {
      controller->puts("region dropped");     
      std::ostringstream str;
      str << "copy_region "
          << rect.left  << " " << rect.top << " " 
          << rect.right << " " << rect.bottom << " "
          << ev.x << " " << ev.y << std::endl;
      server->send(str.str());
    }
  else
    {
      rect.left = ev.x;
      rect.top  = ev.y;
      controller->puts("region select started");
    }
}

void
RegionTool::on_button_release(const ToolButtonEvent& ev)
{
  if (!have_region)
    {
      rect.right  = ev.x;
      rect.bottom = ev.y;

      rect.normalize();
      controller->puts("region selected");
      have_region = true;
    }
  else
    {
      have_region = false;
    }
}

/* EOF */
