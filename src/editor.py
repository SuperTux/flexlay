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

editor_map = EditorMapComponent(CL_Rect(0, 0, 799, 599), gui.get_component())
workspace  = Workspace(799, 599)
editor_map.set_workspace(workspace)

m = EditorMap("Foobar")
workspace.set_current_map(m)
tileset = Tileset(32)
tilemap = EditorTileMap(tileset, 100, 50)
m.add_layer(tilemap)
tile = Tile("/home/ingo/cvs/supertux/supertux/data/images/tilesets/bonus1.png",
            CL_Color(255, 255, 255, 255),
            CL_Color(255,   0,   0, 128))
tileset.add_tile(0, tile)
tileset.add_tile(1, tile)
tileset.add_tile(2, tile)

editor_tilemap_set_current(tilemap)
tilemap_paint_tool_set_tilemap(tilemap)

editor_set_brush_tile(1)

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

window = CL_Window(CL_Rect(50, 50, 350, 250), "My Window", gui.get_component())

gui.push_component(window)
button1 = CL_Button(CL_Rect(50, 50, 200, 75), "Quit", gui.get_component())
connect(button1.sig_clicked(), foo)

button2 = CL_Button(CL_Rect(CL_Point(50, 100), CL_Size(150, 25)), "Draw", gui.get_component())
connect(button2.sig_clicked(), draw_something)
gui.pop_component()

class Menu(CL_Menu):
    def __init__(self):
        CL_Menu.__init__(self, gui.get_component())
    
    def add_item(self, name, func):
        item = self.create_item(name)
        connect(item.sig_clicked(), func)

def menu_file_open():
    print "File/Open"

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

# EOF #
