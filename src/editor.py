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
from supertux import *

flexlay = Flexlay()
flexlay.init()

editor = Editor()
gui = editor.get_gui_manager()

editor_map = EditorMapComponent(CL_Rect(0, 0, 799, 599), gui.get_component())
workspace  = Workspace(799, 599)
editor_map.set_workspace(workspace)

m = EditorMap()
workspace.set_current_map(m)

tileset = load_supertux_tiles()
tilemap = TilemapLayer(tileset, 20, 10)
m.add_layer(tilemap.to_layer())
    
TilemapLayer_set_current(tilemap)

def foo():
    print "---My Callback---"
    gui.quit()

g = None

def draw_something():
    print "Draw something"
    brush = TileBrush(2, 2)
    brush.set_opaque()
    _ = PaintCommand(tilemap, brush)
    _.add_point(CL_Point(1,1))
    _.add_point(CL_Point(2,2))
    _.add_point(CL_Point(3,3))
    _.add_point(CL_Point(4,4))
    _.execute()
    g = _
    print "Draw something done"

window = CL_Window(CL_Rect(50, 50, 350, 300), "My Window", gui.get_component())
    
gui.push_component(window.get_client_area())
button1 = CL_Button(CL_Rect(50, 50, 200, 75), "Quit", gui.get_component())
connect(button1.sig_clicked(), foo)

button2 = CL_Button(CL_Rect(CL_Point(50, 100), CL_Size(150, 25)), "Draw", gui.get_component())
connect(button2.sig_clicked(), draw_something)

load_icon    = Icon(CL_Point(34*0, 0), make_sprite("../data/images/icons/stock_open.png"), "Some tooltip", gui.get_component());
save_icon    = Icon(CL_Point(34*1, 0), make_sprite("../data/images/icons/stock_save.png"), "Some tooltip", gui.get_component());
save_as_icon = Icon(CL_Point(34*2, 0), make_sprite("../data/images/icons/stock_save_as.png"), "Some tooltip", gui.get_component());

copy_icon    = Icon(CL_Point(34*3.1, 0), make_sprite("../data/images/icons/stock_copy.png"), "Some tooltip", gui.get_component());
paste_icon   = Icon(CL_Point(34*4.1, 0), make_sprite("../data/images/icons/stock_paste.png"), "Some tooltip", gui.get_component());

def foo():
    print "Button pressed"

connect(load_icon.sig_clicked(), foo)

gui.get_component()

gui.pop_component()

supertux = SuperTuxGUI(tileset, gui)

class Menu(CL_Menu):
    def __init__(self):
        CL_Menu.__init__(self, gui.get_component())

    def add_item(self, name, func):
            item = self.create_item(name)
            connect(item.sig_clicked(), func)

level = None
def menu_file_open():
    print "File/Open"
    level = SuperTuxLevel('/home/ingo/cvs/supertux/supertux/data/levels/world1/level2.stl')
    print "Loading done"
    level.activate(workspace)
    print "Activation done"

def menu_file_save():
    print "File/Save"

def menu_file_save_as():
    print "File/Save As"

menu = Menu()
a = menu.add_item("File/Open...", menu_file_open)
a = menu.add_item("File/Save...", menu_file_save)
a = menu.add_item("File/Save As...", menu_file_save_as)

gui.run()

flexlay.deinit()
print "deinit done"

# EOF #
