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
from sexpr   import *
import time

time.sleep(1)

def load_game_tiles(tileset, filename):
    "Load game tiles from filename into tileset"
    tree = sexpr_read_from_file(filename)
    tree = tree[1:]
    for i in tree:
        if i[0] == "tile":
            data  = i[1:]
            id    = get_value_from_tree(['id', '_'], data, -1)
            image = get_value_from_tree(['editor-images', '_'], data, False)

            if not(image):
                image = get_value_from_tree(['images', '_'], data, "notile.png")

            tileset.add_tile(id,
                             Tile(supertux_datadir + 'images/tilesets/' + image,
                                  CL_Color(255, 255, 255, 255),
                                  CL_Color(255,   0,   0, 128)))

class SuperTuxLevel:
    me = None
    name   = "no name"
    author = "no author"
    width  = 20
    height = 15

    foreground  = None
    interactive = None
    background  = None

    editormap = None

    def __init__(self, filename):
        print "SuperTuxLevel:__init__"
        self.me = self

        tree = sexpr_read_from_file(filename)
        data = tree[1:]

        self.name   = get_value_from_tree(["name", "_"], data, "no name")
        self.author = get_value_from_tree(["name", "_"], data, "no author")

        self.width  = get_value_from_tree(["width", "_"], data, 20)
        self.height = get_value_from_tree(["height""_"], data, 15)

        self.foreground  = TilemapLayer(tileset, self.width, self.height)
        self.foreground.set_data(get_value_from_tree(["foreground-tm"], data, []))

        self.interactive = TilemapLayer(tileset, self.width, self.height)
        self.interactive.set_data(get_value_from_tree(["interactive-tm"], data, []))

        self.background  = TilemapLayer(tileset, self.width, self.height)
        self.background.set_data(get_value_from_tree(["background-tm"], data, []))

        self.editormap = EditorMap()
        self.editormap.add_layer(self.foreground.to_layer())
        self.editormap.add_layer(self.interactive.to_layer())
        self.editormap.add_layer(self.background.to_layer())

    def __del__(self):
        print "SuperTuxLevel:__del__"

    def activate(self, workspace):
        #editor_tilemap_set_current(self.interactive.to_layer())
        workspace.set_current_map(self.editormap)

class SuperTuxGUI:
    quit_button = None
    menu = None
    tileselector_window = None
    tileselector = None

    def __init__(self):
        self.tileselector_window = CL_Window(CL_Rect(CL_Point(150, 150), CL_Size(210, 210)),
                                             "Tile Selector", gui.get_component())
        self.tileselector = TileSelector(5, 3, self.tileselector_window.get_client_area())
        self.tileselector.set_tileset(tileset)
        self.tileselector.set_tiles(range(1,100))
        
### Begin: 'Main Loop'
supertux_datadir = "/home/ingo/cvs/supertux/supertux/data/"

flexlay = Flexlay()
flexlay.init()

editor = Editor()
gui = editor.get_gui_manager()

tileset = Tileset(32)
load_game_tiles(tileset, "/home/ingo/cvs/supertux/supertux/data/images/tilesets/supertux.stgt")

# tileselector_window = CL_Window(CL_Rect(CL_Point(150, 150), CL_Size(210, 210)),
#                                     "Tile Selector", gui.get_component())
# tileselector = TileSelector(5, 3, tileselector_window.get_client_area())
# tileselector.set_tileset(tileset)
#tileselector.set_tiles(range(1,100))
        
# supertux_gui = SuperTuxGUI()

editor_map = EditorMapComponent(CL_Rect(0, 0, 799, 599), gui.get_component())
workspace  = Workspace(799, 599)
editor_map.set_workspace(workspace)

m = EditorMap()
workspace.set_current_map(m)

tilemap = TilemapLayer(tileset, 20, 10)
m.add_layer(tilemap.to_layer())

# window = CL_Window(CL_Rect(50, 50, 350, 300), "My Window", gui.get_component())
    
print "Launching GUI"
gui.run()

flexlay.deinit()
### End: 'Main Loop'

# EOF #
