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

flexlay_init()

editor = Editor()
gui = editor.get_gui()

editor_map = EditorMapComponent(Rect(0, 0, 799, 599), gui.get_component())
workspace  = Workspace(799, 599)
editor_map.set_workspace(workspace)

m = EditorMap("Foobar")
workspace.set_map(m)
tileset = Tileset(32)
tilemap = TileMap(tileset, 100, 50)
m.add(tilemap)
tile = Tile("/home/ingo/cvs/supertux/supertux/data/images/tilesets/bonus1.png",
            Color(255, 255, 255, 255),
            Color(255,   0,   0, 128))
tileset.add_tile(0, tile)
tileset.add_tile(1, tile)
tileset.add_tile(2, tile)

tilemap_set_current(tilemap)
tilemap_paint_tool_set_tilemap(tilemap)

editor_set_brush_tile(1)

def foo():
    print "---My Callback---"
    gui.quit()

window = Window(Rect(50, 50, 350, 250), "My Window", gui.get_component())

gui.push_component(window)
button = Button(Rect(50, 50, 200, 75), "Quit", gui.get_component())
connect(button.sig_clicked(), foo)
gui.pop_component()

button2 = Button(Rect(0, 0, 100, 25), "Quit", gui.get_component())
connect(button2.sig_clicked(), foo)

menu = Menu(gui.get_component());
a = menu.add_item("File/Open...", "File/Open...")
a = menu.add_item("File/Save...", "File/Save...")
a = menu.add_item("File/Save As...", "File/Save As...")
c = menu.foobar()
gui.run()

flexlay_deinit()

# EOF #
