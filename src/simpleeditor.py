##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License
##  as published by the Free Software Foundation; either version 2
##  of the License, or (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

from flexlay import *

flexlay = Flexlay()

flexlay.init()

editor = Editor()
gui = editor.get_gui_manager()

def get_data():
    print "I got clicked"

m = EditorMap();
tileset = Tileset(32);
tilemap = TilemapLayer(tileset, 20, 10);
m.add_layer(tilemap.to_layer())

TilemapLayer_set_current(tilemap)
  
editor_map = EditorMapComponent(CL_Rect(0, 0, 799, 599), gui.get_component())
workspace = Workspace(799, 599)
editor_map.set_workspace(workspace)
workspace.set_current_map(m)

button = CL_Button(CL_Rect(CL_Point(50, 150), CL_Size(150, 25)),
                   "Get Data", gui.get_component())
connect(button.sig_clicked(), get_data)

def do_quit():
    gui.quit()
    
quit = CL_Button(CL_Rect(CL_Point(50, 250), CL_Size(150, 25)),
                   "Do Quit", gui.get_component())
connect(quit.sig_clicked(), do_quit)
  
gui.run()

flexlay.deinit()

# EOF #
