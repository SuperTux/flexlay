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
import ConfigParser
import os

class Config:
    config = None
    datadir = None
    recent_files = []
    
    def __init__(self):
        self.config = ConfigParser.ConfigParser()

        # Setting defaults
        self.config.add_section("SuperTux")
        self.config.set("SuperTux", "datadir", "/home/ingo/cvs/supertux/supertux/data/")
        self.config.set("SuperTux", "recent_files", [])

        self.config.read(['supertux.cfg', os.path.expanduser('~/.flexlay/supertux.cfg')])

        self.datadir      = self.config.get("SuperTux", "datadir")
        self.recent_files = eval(self.config.get("SuperTux", "recent_files"))
        
    def __del__(self):
        self.config.set("SuperTux", "datadir", self.datadir)
        self.config.set("SuperTux", "recent_files", self.recent_files)
        
        self.config.write(open(os.path.expanduser('~/.flexlay/supertux.cfg'), 'w'))

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
                             Tile(config.datadir + 'images/tilesets/' + image,
                                  CL_Color(254, 254, 254, 254),
                                  CL_Color(255,   0,   0, 128)))

class SuperTuxLevel:
    filename = None
    
    name   = "no name"
    author = "no author"
    width  = 200
    height = 100
    gravity = 10
    theme = "antarctica"
    time = 999
    music = "Mortimers_chipdisko.mod"

    foreground  = None
    interactive = None
    background  = None
    objects = None
    camera  = None

    editormap = None

    def __init__(self, *params):
        if len(params) == 2:
            (width, height) = params
            
            self.name   = "No Name"
            self.author = "No Author"

            self.width  = width
            self.height = height

            self.foreground  = TilemapLayer(supertux_tileset, self.width, self.height)
            self.interactive = TilemapLayer(supertux_tileset, self.width, self.height)
            self.background  = TilemapLayer(supertux_tileset, self.width, self.height)
            self.objects = ObjectLayer()
            
        elif len(params) == 1:
            (self.filename,) = params
            
            tree = sexpr_read_from_file(self.filename)
            if tree == None:
                raise "Couldn't load level"
            
            data = tree[1:]

            self.name   = get_value_from_tree(["name", "_"], data, "no name")
            self.author = get_value_from_tree(["name", "_"], data, "no author")

            self.width  = get_value_from_tree(["width", "_"], data, 20)
            self.height = get_value_from_tree(["height""_"], data, 15)

            self.foreground  = TilemapLayer(supertux_tileset, self.width, self.height)
            self.foreground.set_data(get_value_from_tree(["foreground-tm"], data, []))

            self.interactive = TilemapLayer(supertux_tileset, self.width, self.height)
            self.interactive.set_data(get_value_from_tree(["interactive-tm"], data, []))

            self.background  = TilemapLayer(supertux_tileset, self.width, self.height)
            self.background.set_data(get_value_from_tree(["background-tm"], data, []))

            self.objects = ObjectLayer()
            
        else:
            raise "Wrong arguments for SuperTux::___init__"

        self.editormap = EditorMap()
        self.editormap.add_layer(self.background.to_layer())
        self.editormap.add_layer(self.interactive.to_layer())
        self.editormap.add_layer(self.objects.to_layer())
        self.editormap.add_layer(self.foreground.to_layer())
        # FIXME: Data might not get freed since its 'recursively' refcounted
        self.editormap.set_metadata(make_metadata(self))

    def save(self, filename):
        f = file(filename, 'w')
        f.write(";; Generated by Flexlay Editor\n"
                "(supertux-level\n")
        f.write("  (version 1)\n")
        f.write("  (name   \"%s\")\n" % self.name)
        f.write("  (author \"%s\")\n" % self.author)
        f.write("  (width  %d)\n"  % self.width)
        f.write("  (height  %d)\n" % self.height)

        f.write("  (music  \"%s\")\n" % self.music)
        f.write("  (time   \"%s\")\n" % self.time)

        f.write("  (gravity %d)\n" % self.gravity)

        f.write("  (theme \"%s\")\n" % self.theme)

        f.write("  (interactive-tm\n")
        for i in self.interactive.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")

        f.write("  (background-tm\n")
        for i in self.background.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")

        f.write("  (foreground-tm\n")
        for i in self.foreground.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")
        f.write(" )\n\n;; EOF ;;\n")

        # objects = None

    def activate(self, workspace):
        workspace.set_map(self.editormap)
        TilemapLayer.set_current(self.interactive)
        ObjectLayer.set_current(self.objects)
        #(tilemap-paint-tool-set-tilemap (supertux:interactive-tm stlv))
        #(editor-tilemap-set-current     (supertux:interactive-tm stlv))
        #(editor-objectmap-set-current   (supertux:objmap stlv))
        #(set! *tilemap* (supertux:interactive-tm stlv))
        #(set! *objmap* (supertux:objmap stlv))
        #(tileset-set-current *level-tileset*)
        #(tile-selector-set-tileset *tileselector* *level-tileset*))

SuperTuxLevel.BACKGROUND  = 0
SuperTuxLevel.INTERACTIVE = 1
SuperTuxLevel.FOREGROUND  = 2

class DisplayProperties:
    layer = SuperTuxLevel.INTERACTIVE
    show_all = False
    current_only = False
    
    def set(self, map):
        if self.current_only:
            active   = CL_Color(255, 255, 255)
            deactive = CL_Color(150, 150, 250, 150)
        else:
            active   = CL_Color(255, 255, 255)
            deactive = CL_Color(0, 0, 0, 10)

        if (self.show_all):
            map.foreground.set_foreground_color(active)
            map.interactive.set_foreground_color(active)
            map.background.set_foreground_color(active)
        else:
            if (self.layer == SuperTuxLevel.FOREGROUND):
                map.foreground.set_foreground_color(active)
            else:
                map.foreground.set_foreground_color(deactive)

            if (self.layer == SuperTuxLevel.INTERACTIVE):
                map.interactive.set_foreground_color(active)
            else:
                map.interactive.set_foreground_color(deactive)

            if (self.layer == SuperTuxLevel.BACKGROUND):
                map.background.set_foreground_color(active)
            else:
                map.background.set_foreground_color(deactive)

class SuperTuxGUI:
    quit_button = None
    menu        = None
    
    selector_window = None
    tileselector    = None
    objectselector  = None

    def __init__(self, tileset, gui):
        self.selector_window = Panel(CL_Rect(CL_Point(800-134, 23+33), CL_Size(128 + 6, 558)),
                                         gui.get_component())
        self.tileselector = TileSelector(CL_Rect(CL_Point(3, 3), CL_Size(128, 552)), self.selector_window)
        self.tileselector.set_tileset(tileset)
        self.tileselector.set_tiles(range(1,100))
        self.tileselector.show(False)

        self.objectselector = ObjectSelector(CL_Rect(0, 0, 128, 256), 42, 42, self.selector_window)
        self.objectselector.show(True)
        self.objectselector.add_brush(ObjectBrush(make_sprite("../data/images/tools/stock-tool-pencil-22.png"),
                                                  make_metadata(None)))
        self.objectselector.add_brush(ObjectBrush(make_sprite("../data/images/tools/stock-tool-pencil-22.png"),
                                                  make_metadata(None)))
        self.objectselector.add_brush(ObjectBrush(make_sprite("../data/images/tools/stock-tool-pencil-22.png"),
                                                  make_metadata(None)))        
        self.objectselector.add_brush(ObjectBrush(make_sprite("../data/images/tools/stock-tool-pencil-22.png"),
                                                  make_metadata(None)))
        self.objectselector.add_brush(ObjectBrush(make_sprite("../data/images/tools/stock-tool-pencil-22.png"),
                                                  make_metadata(None)))

    def show_objects(self):
        self.tileselector.show(False)        
        self.objectselector.show(True)

    def show_tiles(self):
        self.tileselector.show(True)        
        self.objectselector.show(False)

    def show_none(self):
        self.tileselector.show(False)        
        self.objectselector.show(False)
        
def load_supertux_tiles():
    tileset = Tileset(32)
    load_game_tiles(tileset, "/home/ingo/cvs/supertux/supertux/data/images/tilesets/supertux.stgt")
    return tileset 

config = Config()
supertux_tileset = load_supertux_tiles()
    
### End: 'Main Loop'

# EOF #
