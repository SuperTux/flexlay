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

supertux_datadir = "/home/ingo/cvs/supertux/supertux/data/"

def assoc_ref(lst, str):
    if lst == []:
        return False
    elif lst[0][0] == str:
        return lst[0][1:]
    else:
        return assoc_ref(lst[1:], str)

def get_value_from_tree(spec, tree, default):
    if spec == []:
        return tree
    elif spec == ['_']:
        return tree[0]
    elif tree == []:
        return default
    else:
        el = assoc_ref(tree, spec[0])
        if el:
            return get_value_from_tree(spec[1:], el, default)
        else:
            return default

def load_game_tiles(tileset, filename):
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


flexlay = Flexlay()

flexlay.init()

tileset = Tileset(32)
load_game_tiles(tileset, "/home/ingo/cvs/supertux/supertux/data/images/tilesets/supertux.stgt")

def do_editor():
    editor = Editor()
    gui = editor.get_gui_manager()
    
    editor_map = EditorMapComponent(CL_Rect(0, 0, 799, 599), gui.get_component())
    workspace  = Workspace(799, 599)
    editor_map.set_workspace(workspace)
    
    m = EditorMap()
    workspace.set_current_map(m)

    tilemap = TilemapLayer(tileset, 20, 10)
    m.add_layer(tilemap.to_layer())
        
    TilemapLayer_set_current(tilemap)
    
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

    window = CL_Window(CL_Rect(50, 50, 350, 300), "My Window", gui.get_component())
        
    gui.push_component(window)
    button1 = CL_Button(CL_Rect(50, 50, 200, 75), "Quit", gui.get_component())
    connect(button1.sig_clicked(), foo)
    
    button2 = CL_Button(CL_Rect(CL_Point(50, 100), CL_Size(150, 25)), "Draw", gui.get_component())
    connect(button2.sig_clicked(), draw_something)
    
    def get_data():
        print tilemap.get_data()
        
    def set_data():
        tilemap.set_data((0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0))

    button3 = CL_Button(CL_Rect(CL_Point(50, 150), CL_Size(150, 25)), "Get Data", gui.get_component())
    connect(button3.sig_clicked(), get_data)
    
    button4 = CL_Button(CL_Rect(CL_Point(50, 180), CL_Size(150, 25)), "Get Data", gui.get_component())
    connect(button4.sig_clicked(), set_data)
    
    gui.pop_component()
    
    tileselectorw = CL_Window(CL_Rect(CL_Point(150, 150), CL_Size(210, 210)), "Tile Selector", gui.get_component())
    tileselector = TileSelector(5, 3, tileselectorw.get_client_area())
    tileselector.set_tileset(tileset)
    tileselector.set_tiles(range(1,100))

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

do_editor()

flexlay.deinit()
print "deinit done"

# EOF #
