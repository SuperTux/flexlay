#! /usr/bin/env python2.2
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
import ConfigParser
import os
import sys
import code
from optparse import OptionParser

def get_completions(text):
    import rlcompleter
    comp = rlcompleter.Completer()
    i = 0
    ret = []
    while True:
        line = comp.complete(text, i)
        if line == None:
            break;
        
        ret.append(line)           
        i += 1
    return ret

def run_python():
    repl = code.InteractiveConsole()
    repl.runsource("import readline")
    repl.runsource("import rlcompleter")
    repl.runsource("readline.parse_and_bind('tab: complete')")
    repl.interact("Use Ctrl-D to exit subshell")
    print "### Interactive Console finished"

game_objects = [["money", "images/shared/jumpy-left-middle-0.png"],
                ["snowball", "images/shared/snowball-left-0.png"],
                ["mriceblock", "images/shared/mriceblock-left-0.png"],
                ["mrbomb", "images/shared/mrbomb-left-0.png"],
                ["flame", "images/shared/flame-0.png"], 
                ["stalactite", "images/shared/stalactite.png"],
                ["fish", "images/shared/fish-left-0.png"],
                ["flyingsnowball", "images/shared/flyingsnowball-left-0.png"],
                ["bouncingsnowball", "images/shared/bouncingsnowball-left-0.png"],
                ["spiky", "images/shared/spiky-left-0.png"],
                ["resetpoint", "images/shared/resetpoint.png"]]

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
        str = self.config.get("SuperTux", "recent_files")
        print str
        self.recent_files = eval(str)
        
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

            if id != 0: # leave tile 0 transparent
                tileset.add_tile(id,
                                 Tile(config.datadir + 'images/tilesets/' + image,
                                      CL_Color(255,   0,   0, 128)))

class BadGuy:
    type = None

    def __init__(self,  type):
        self.type = type
    

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
                raise ("Couldn't load level: ", filename)
            
            data = tree[1:]

            self.name   = get_value_from_tree(["name", "_"], data, "no name")
            self.author = get_value_from_tree(["author", "_"], data, "no author")

            self.width  = get_value_from_tree(["width", "_"], data, 20)
            self.height = get_value_from_tree(["height""_"], data, 15)

            self.foreground  = TilemapLayer(supertux_tileset, self.width, self.height)
            self.foreground.set_data(get_value_from_tree(["foreground-tm"], data, []))

            self.interactive = TilemapLayer(supertux_tileset, self.width, self.height)
            self.interactive.set_data(get_value_from_tree(["interactive-tm"], data, []))

            self.background  = TilemapLayer(supertux_tileset, self.width, self.height)
            self.background.set_data(get_value_from_tree(["background-tm"], data, []))

            def find(lst, obj):
                for i in lst:
                    if i[0] == obj:
                        return i
                return None
            
            self.objects = ObjectLayer()
            for i in get_value_from_tree(["objects"], data, []):
                type = i[0]
                x = get_value_from_tree(["x", "_"], i[1:], [])
                y = get_value_from_tree(["y", "_"], i[1:], [])
                object = find(game_objects, type)
                self.objects.add_object(ObjMapSpriteObject(make_sprite(config.datadir + object[1]),
                                                           CL_Point(x, y),
                                                           make_metadata(BadGuy(object[0]))).to_object())
                
            for i in get_value_from_tree(["reset-points"], data, []):
                type = i[0]
                x = get_value_from_tree(["x", "_"], i[1:], [])
                y = get_value_from_tree(["y", "_"], i[1:], [])
                object = find(game_objects, "resetpoint")
                self.objects.add_object(ObjMapSpriteObject(make_sprite(config.datadir + object[1]),
                                                           CL_Point(x, y),
                                                           make_metadata(BadGuy(object[0]))).to_object())
           
        else:
            raise "Wrong arguments for SuperTux::___init__"

        self.editormap = EditorMap()
        self.editormap.add_layer(self.background.to_layer())
        self.editormap.add_layer(self.interactive.to_layer())
        self.editormap.add_layer(self.objects.to_layer())
        self.editormap.add_layer(self.foreground.to_layer())
        # FIXME: Data might not get freed since its 'recursively' refcounted
        self.editormap.set_metadata(make_metadata(self))

    def resize(self, size, pos):
        self.width  = size.width
        self.height = size.height
        self.background.resize(size, pos)
        self.interactive.resize(size, pos)
        self.foreground.resize(size, pos)
        
    def save(self, filename):
        f = file(filename, 'w')
        f.write(";; Generated by Flexlay Editor\n"
                "(supertux-level\n")
        f.write("  (version 1)\n")
        f.write("  (name   \"%s\")\n" % self.name)
        f.write("  (author \"%s\")\n" % self.author)
        f.write("  (width  %s)\n"  % self.width)
        f.write("  (height  %s)\n" % self.height)

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

        f.write("  (objects\n")
        for obj in self.objects.get_objects():
            badguy = get_python_object(obj.get_metadata())
            pos    = obj.get_pos()
            if (badguy.type != "resetpoint"):
                f.write("     (%s (x %d) (y %d))\n" % (badguy.type, int(pos.x), int(pos.y)))
        f.write("  )\n\n")

        f.write("  (reset-points\n")
        for obj in self.objects.get_objects():
            badguy = get_python_object(obj.get_metadata())
            pos    = obj.get_pos()
            if (badguy.type == "resetpoint"):
                f.write("     (point (x %d) (y %d))\n" % (int(pos.x), int(pos.y)))
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
            deactive = CL_Color(0, 0, 0, 10)
        else:
            active   = CL_Color(255, 255, 255)
            deactive = CL_Color(150, 150, 250, 150)

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
        for object in game_objects:
            self.objectselector.add_brush(ObjectBrush(make_sprite(config.datadir + object[1]),
                                                      make_metadata(BadGuy(object[0]))))

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

def has_element(lst, el):
    for i in lst:
        if i == el:
            return True
    return False

def supertux_load_level(filename):   
    print "Loading: ", filename
    level = SuperTuxLevel(filename)
    level.activate(workspace)
    connect(level.editormap.sig_change(), on_map_change)
    
    if not(has_element(config.recent_files, filename)):
        config.recent_files.append(filename)
        recent_files_menu.add_item(mysprite, filename, lambda: supertux_load_level(filename))

    minimap.update_minimap()

# Begin: Main loop
parser = OptionParser()
parser.add_option("-f", "--file", dest="filename",
                  help="write report to FILE", metavar="FILE")
parser.add_option("-q", "--quiet",
                  action="store_false", dest="verbose", default=True,
                  help="don't print status messages to stdout")

(options, args) = parser.parse_args()

print "SUperTUX"
flexlay = Flexlay()
flexlay.init()
print "..SUperTUX"
config = Config()
supertux_tileset = load_supertux_tiles()

screen_w = 800
screen_h = 600

editor = Editor()
gui = editor.get_gui_manager()

myrect = CL_Rect(CL_Point(0, 56), CL_Size(665, 488+56))
editor_map = EditorMapComponent(myrect, gui.get_component())
workspace  = Workspace(myrect.get_width(), myrect.get_height())
editor_map.set_workspace(workspace)

# Tools
tilemap_paint_tool  = TileMapPaintTool()
tilemap_select_tool = TileMapSelectTool()
zoom_tool           = ZoomTool()
objmap_select_tool  = ObjMapSelectTool()

workspace.set_tool(tilemap_paint_tool.to_tool());

def on_map_change():
    if (workspace.get_map().undo_stack_size() > 0):
        undo_icon.enable()
    else:
        undo_icon.disable()

    if (workspace.get_map().redo_stack_size() > 0):
        redo_icon.enable()
    else:
        redo_icon.disable()        

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

recent_files_menu = Menu(CL_Point(32*2, 54), gui.get_component())
for filename in config.recent_files:
    recent_files_menu.add_item(mysprite, filename, lambda: supertux_load_level(filename))

minimap = Minimap(editor_map, CL_Rect(CL_Point(3, 488+3-14), CL_Size(794-134-16, 50)), editor_map)

if args == []:
    startlevel = SuperTuxLevel(100, 50)
    # startlevel = netpanzer.Level(256, 256)
    startlevel.activate(workspace)
    connect(startlevel.editormap.sig_change(), on_map_change)
else:
    supertux_load_level(args[0])

button_panel = Panel(CL_Rect(CL_Point(0, 23), CL_Size(800, 33)), gui.get_component())

def gui_level_save_as():
    save_dialog.set_filename(os.path.dirname(save_dialog.get_filename()) + "/")
    save_dialog.run(supertux_save_level)

def gui_level_save():
    if workspace.get_map().get_metadata().filename:
        save_dialog.set_filename(workspace.get_map().get_metadata().filename)
    else:
        save_dialog.set_filename(os.path.dirname(save_dialog.get_filename())  + "/")
        
    save_dialog.run(supertux_save_level)
   
def gui_level_load():
    load_dialog.run(supertux_load_level)

def gui_toggle_minimap():
    if minimap.is_visible():
        minimap.show(False)
        minimap_icon.set_up()
    else:
        minimap.show(True)
        minimap_icon.set_down()

def gui_toggle_grid():
    tilemap = workspace.get_map().get_metadata().foreground;
    tilemap.set_draw_grid(not(tilemap.get_draw_grid()))
    if tilemap.get_draw_grid():
        grid_icon.set_down()
    else:
        grid_icon.set_up()

class Counter:
    counter = 0;
    
    def __init__(self, i):
        self.counter = i
        
    def inc(self, i):
        self.counter += i
        return self.counter

p = Counter(2)

new_icon         = Icon(CL_Rect(CL_Point(p.inc(0),  2), CL_Size(32, 32)),
                                make_sprite("../data/images/icons24/stock_new.png"), "Some tooltip", button_panel);
load_icon        = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
                        make_sprite("../data/images/icons24/stock_open.png"), "Some tooltip", button_panel);
load_recent_icon = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(16, 32)),
                        make_sprite("../data/images/icons24/downarrow.png"), "Some tooltip", button_panel);
save_icon        = Icon(CL_Rect(CL_Point(p.inc(16), 2), CL_Size(32, 32)),
                        make_sprite("../data/images/icons24/stock_save.png"), "Some tooltip", button_panel);
save_as_icon     = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
                        make_sprite("../data/images/icons24/stock_save_as.png"), "Some tooltip", button_panel);

load_icon.set_callback(gui_level_load)
load_recent_icon.set_callback(lambda: recent_files_menu.run())
save_icon.set_callback(gui_level_save)
save_as_icon.set_callback(gui_level_save_as)

copy_icon    = Icon(CL_Rect(CL_Point(p.inc(48), 2), CL_Size(32, 32)),
                    make_sprite("../data/images/icons24/stock_copy.png"), "Some tooltip", button_panel);
paste_icon   = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
                    make_sprite("../data/images/icons24/stock_paste.png"), "Some tooltip", button_panel);

undo_icon = Icon(CL_Rect(CL_Point(p.inc(48), 2), CL_Size(32, 32)),
                 make_sprite("../data/images/icons24/stock_undo.png"), "Some tooltip", button_panel);
redo_icon = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
                 make_sprite("../data/images/icons24/stock_redo.png"), "Some tooltip", button_panel);

undo_icon.set_callback(lambda: workspace.get_map().undo())
redo_icon.set_callback(lambda: workspace.get_map().redo())

undo_icon.disable()
redo_icon.disable()

minimap_icon = Icon(CL_Rect(CL_Point(p.inc(48), 2), CL_Size(32, 32)),
                    make_sprite("../data/images/icons24/minimap.png"), "Some tooltip", button_panel);
minimap_icon.set_callback(gui_toggle_minimap)

grid_icon = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
                 make_sprite("../data/images/icons24/grid.png"), "Some tooltip", button_panel);
grid_icon.set_callback(gui_toggle_grid)

background_icon  = Icon(CL_Rect(CL_Point(p.inc(48), 2), CL_Size(32, 32)),
                        make_sprite("../data/images/icons24/background.png"), "Some tooltip", button_panel);
interactive_icon = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
                        make_sprite("../data/images/icons24/interactive.png"), "Some tooltip", button_panel);
foreground_icon  = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
                        make_sprite("../data/images/icons24/foreground.png"), "Some tooltip", button_panel);
eye_icon         = Icon(CL_Rect(CL_Point(p.inc(32), 2), CL_Size(32, 32)),
                        make_sprite("../data/images/icons24/eye.png"), "Some tooltip", button_panel);

layer_menu = Menu(CL_Point(32*15+2, 54), gui.get_component())

def set_tilemap_paint_tool():
    workspace.set_tool(tilemap_paint_tool.to_tool())
    paint.set_down()
    select.set_up()
    zoom.set_up()
    object.set_up()
    supertux.show_tiles()

def set_tilemap_select_tool():
    workspace.set_tool(tilemap_select_tool.to_tool())
    paint.set_up()
    select.set_down()
    zoom.set_up()
    object.set_up()
    supertux.show_none()
    
def set_zoom_tool():
    workspace.set_tool(zoom_tool.to_tool())
    paint.set_up()
    select.set_up()
    zoom.set_down()
    object.set_up()
    supertux.show_none()
    
def set_objmap_select_tool():
    workspace.set_tool(objmap_select_tool.to_tool())
    paint.set_up()
    select.set_up()
    zoom.set_up()
    object.set_down()
    supertux.show_objects()

toolbar = Panel(CL_Rect(CL_Point(0, 23+33), CL_Size(33, 32*4+2)), gui.get_component())

paint = Icon(CL_Rect(CL_Point(2, 32*0+2), CL_Size(32, 32)), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", toolbar);
paint.set_callback(set_tilemap_paint_tool)

select = Icon(CL_Rect(CL_Point(2, 32*1+2), CL_Size(32,32)), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", toolbar);
select.set_callback(set_tilemap_select_tool)

zoom = Icon(CL_Rect(CL_Point(2, 32*2+2), CL_Size(32,32)), make_sprite("../data/images/tools/stock-tool-zoom-22.png"), "Some tooltip", toolbar);
zoom.set_callback(set_zoom_tool)

object = Icon(CL_Rect(CL_Point(2, 32*3+2), CL_Size(32,32)), make_sprite("../data/images/tools/stock-tool-clone-22.png"), "Some tooltip", toolbar);
object.set_callback(set_objmap_select_tool)

# erase  = Icon(CL_Point(2, 32+1+2), make_sprite("../data/images/tools/stock-tool-eraser-22.png"), "Some tooltip", toolbar);
# move   = Icon(CL_Point(2, 32*2+2), make_sprite("../data/images/tools/stock-tool-move-22.png"), "Some tooltip", toolbar);

# SuperTux Specific stuff
def gui_show_foreground():
    display_properties.layer = SuperTuxLevel.FOREGROUND
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().foreground)
    foreground_icon.set_down()
    interactive_icon.set_up()
    background_icon.set_up()
    minimap.update_minimap()

def gui_show_background():
    display_properties.layer = SuperTuxLevel.BACKGROUND
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().background)
    foreground_icon.set_up()
    interactive_icon.set_up()
    background_icon.set_down()
    minimap.update_minimap()

def gui_show_interactive():
    display_properties.layer = SuperTuxLevel.INTERACTIVE
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().interactive)
    foreground_icon.set_up()
    interactive_icon.set_down()
    background_icon.set_up()
    minimap.update_minimap()

def gui_show_all():
    display_properties.show_all = True
    display_properties.current_only = False
    display_properties.set(workspace.get_map().get_metadata())

def gui_show_current():
    display_properties.show_all = False
    display_properties.current_only = False
    display_properties.set(workspace.get_map().get_metadata())

def gui_show_only_current():
    display_properties.show_all = False
    display_properties.current_only = True
    display_properties.set(workspace.get_map().get_metadata())

def gui_toggle_display_props():
    if display_properties.show_all:
        display_properties.show_all = False
    elif not(display_properties.current_only):
        display_properties.current_only = True
    else:
         display_properties.show_all = True
         display_properties.current_only = False
        
    display_properties.set(workspace.get_map().get_metadata())    

def gui_resize_level():
    level = workspace.get_map().get_data()
    dialog = GenericDialog("Resize Level", gui.get_component())
    dialog.add_int("Width: ", level.width)
    dialog.add_int("Height: ", level.height)
    dialog.add_int("X: ", 0)
    dialog.add_int("Y: ", 0)
    def resize_callback(w, h, x, y):
        level.resize(CL_Size(w, h), CL_Point(x, y))
    dialog.set_callback(resize_callback)

def gui_resize_level_to_selection():
    level = workspace.get_map().get_data()
    rect  = tilemap_select_tool.get_selection_rect()
    if (rect.get_width() > 2 and rect.get_height() > 2):
        level.resize(rect.get_size(), CL_Point(-rect.left, -rect.top))

foreground_icon.set_callback(gui_show_foreground)
interactive_icon.set_callback(gui_show_interactive)
background_icon.set_callback(gui_show_background)
eye_icon.set_callback(layer_menu.run)

layer_menu.add_item(mysprite, "Show all", gui_show_all)
layer_menu.add_item(mysprite, "Show current", gui_show_current)
layer_menu.add_item(mysprite, "Show only current", gui_show_only_current)

supertux = SuperTuxGUI(load_supertux_tiles(), gui)
# supertux.tileselector.set_tileset(netpanzer.tileset)

level = None
def menu_file_open():
    print "File/Open"
    level = SuperTuxLevel('/home/ingo/cvs/supertux/supertux/data/levels/world1/level2.stl')
    print "Loading done"
    level.activate(workspace)
    connect(level.editormap.sig_change(), on_map_change)
    print "Activation done"

def supertux_save_level(filename):
    workspace.get_map().get_metadata().save(filename)

def netpanzer_load_level(filename):
    NetPanzerFileStruct(filename)

menu = CL_Menu(gui.get_component())
menu.add_item("File/Open...", gui_level_load)
menu.add_item("File/Save...", gui_level_save)
# menu.add_item("File/Save Commands...", menu_file_save_commands)
menu.add_item("File/Save As...", gui_level_save_as)
menu.add_item("File/Quit",  gui.quit)

menu.add_item("Edit/Resize", lambda: gui_resize_level())
menu.add_item("Edit/Resize to selection", lambda: gui_resize_level_to_selection())
menu.add_item("Edit/Debug Shell", lambda: run_python())

def gui_set_zoom(zoom):
    gc = editor_map.get_workspace().get_gc_state()
    pos = gc.get_pos()
    gc.set_zoom(zoom)
    gc.set_pos(pos)

menu.add_item("Zoom/1:4 (25%) ",  lambda: gui_set_zoom(0.25))
menu.add_item("Zoom/1:2 (50%) ",  lambda: gui_set_zoom(0.5))
menu.add_item("Zoom/1:1 (100%) ", lambda: gui_set_zoom(1.0)) 
menu.add_item("Zoom/2:1 (200%) ", lambda: gui_set_zoom(2.0))
menu.add_item("Zoom/4:1 (400%) ", lambda: gui_set_zoom(4.0))

display_properties = DisplayProperties()

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

mymenu = Menu(CL_Point(134, 54), gui.get_component())
mymenu.add_item(mysprite, "Foobar aeuaeu", None)
mymenu.add_item(mysprite, "bla", None)
mymenu.add_seperator()
mymenu.add_item(mysprite, "Foobar", None)
mymenu.add_item(mysprite, "blubaoeuau aueau aeu", None)
mymenu.add_item(mysprite, "bla", None)

load_dialog = SimpleFileDialog("Load SuperTux Level", "Load", "Cancel", gui.get_component())
load_dialog.set_filename(config.datadir + "levels/")
save_dialog = SimpleFileDialog("Save SuperTux Level as...", "Save", "Cancel", gui.get_component())
save_dialog.set_filename(config.datadir + "levels/")

# Init the GUI, so that button state is in sync with internal state
gui_toggle_minimap()
gui_toggle_minimap()
gui_show_interactive()
gui_show_current()
set_tilemap_paint_tool()

connect(editor_map.sig_on_key("m"), lambda: gui_toggle_minimap())
connect(editor_map.sig_on_key("g"), lambda: gui_toggle_grid())
connect(editor_map.sig_on_key("4"), lambda: gui_toggle_display_props())
connect(editor_map.sig_on_key("3"), lambda: gui_show_foreground())
connect(editor_map.sig_on_key("2"), lambda: gui_show_interactive())
connect(editor_map.sig_on_key("1"), lambda: gui_show_background())

gui.run()

del config

flexlay.deinit()
print "deinit done"
    
### End: 'Main Loop'

# EOF #
