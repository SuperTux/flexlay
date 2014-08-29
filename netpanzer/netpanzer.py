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
from netpanzerbrushes import *
import os
import sys
import ConfigParser

class Config:
    config = None
    datadir = None
    recent_files = []
    
    def __init__(self):
        self.config = ConfigParser.ConfigParser()

        # Setting defaults
        self.config.add_section("netPanzer")
        self.config.set("netPanzer", "datadir", "/home/ingo/projects/netpanzer/cvs/netpanzer/")
        self.config.set("netPanzer", "recent_files", "[]")

        self.config.read(['netpanzer.cfg', os.path.expanduser('~/.flexlay/netpanzer.cfg')])

        self.datadir      = self.config.get("netPanzer", "datadir")
        str = self.config.get("netPanzer", "recent_files")
        self.recent_files = eval(str)
        
    def __del__(self):
        self.config.set("netPanzer", "datadir", self.datadir)
        self.config.set("netPanzer", "recent_files", self.recent_files)
        print "Writing config",
        self.config.write(open(os.path.expanduser('~/.flexlay/netpanzer.cfg'), 'w'))
        print "Writing Done"

class Level:
    filename  = None
    data      = None
    editormap = None
    objects   = None

    def __init__(self, *params):
        if len(params) == 2:
            (width, height) = params
            self.data = NetPanzerFileStruct(tileset, width, height)

        elif len(params) == 1:
            (self.filename,) = params
            self.data = NetPanzerFileStruct(tileset, self.filename)

        self.objects = ObjectLayer()
        self.editormap = EditorMap()
        self.editormap.add_layer(self.data.get_tilemap().to_layer())
        self.editormap.add_layer(self.objects.to_layer())

        # FIXME: Data might not get freed since its 'recursively' refcounted
        self.editormap.set_metadata(make_metadata(self))

    def save_optfile(self, filename):
        outpots = [] # FIXME
        
        f = open(filename, "w")
        f.write("ObjectiveCount: %d\n\n" % len(outposts))
        for (name, x , y) in outpots:
            f.write("Name: %s\n" % "Foobar")
            f.write("Location: %d %d\n\n" % (int(x)/32, int(y)/32))

    def save_spnfile(self, filename):
        spawnpoints = []
        f = open(filename, "w")

        f.write("SpawnCount: %d\n\n" % len(spawnpoints))
        for (x, y) in spawnpoints:
            f.write("Location: %d %d\n" % (int(x)/32, int(y)/32))

    def save(self, filename):
        if filename[-4:] == ".npm":
            data.save(filename)
            save_optfile(filename[:-4] + ".opt")
            save_optfile(filename[:-4] + ".spn")
        else:
            raise "Fileextension not valid, must be .npm!"

    def activate(self, workspace):
        workspace.set_map(self.editormap)
        TilemapLayer.set_current(self.data.get_tilemap())
        ObjectLayer.set_current(self.objects)

flexlay = Flexlay()
flexlay.init()

config = Config()

tileset = Tileset(32)
load_netpanzer_tiles(tileset)

screen_w = 800
screen_h = 600

editor = Editor()
gui = editor.get_gui_manager()

myrect = CL_Rect(CL_Point(0, 56), CL_Size(665, 488+56))
editor_map = EditorMapComponent(myrect, gui.get_component())
workspace  = Workspace(myrect.get_width(), myrect.get_height())
editor_map.set_workspace(workspace)

option_panel = Panel(CL_Rect(CL_Point(666, 56), CL_Size(134, 488+56)), gui.get_component())

brushbox = CL_ListBox(CL_Rect(CL_Point(3, 3), CL_Size(128, 488+56-128-9)), option_panel)
for i in brushes:
    (index, width, height, name) = i
    brushbox.insert_item("%s - %sx%s" % (name, width, height))

def brushbox_change(index):
    (start, width,  height, name) = brushes[index]
    brush = TileBrush(width, height)
    brush.set_data(range(start, start + width*height))
    tilemap_paint_tool.set_brush(brush)

connect_v1(brushbox.sig_highlighted(), brushbox_change)

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

startlevel = Level(256, 256)
startlevel.activate(workspace)
connect(startlevel.editormap.sig_change(), on_map_change)

button_panel = Panel(CL_Rect(CL_Point(0, 23), CL_Size(800, 33)), gui.get_component())

def netpanzer_load_level(filename):
    level = Level(filename)
    level.activate(workspace)

def netpanzer_save_level(filename):
    workspace.get_map().get_metadata().save(filename)

def gui_level_save_as():
    save_dialog.set_filename(os.path.dirname(save_dialog.get_filename()) + "/")
    save_dialog.run(netpanzer_save_level)

def gui_level_save():
    if workspace.get_map().get_metadata().filename:
        save_dialog.set_filename(workspace.get_map().get_metadata().filename)
    else:
        save_dialog.set_filename(os.path.dirname(save_dialog.get_filename())  + "/")
        
    save_dialog.run(netpanzer_save_level)
   
def gui_level_load():
    load_dialog.run(netpanzer_load_level)

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

def gui_toggle_grid():
    tilemap = workspace.get_map().get_metadata().data.get_tilemap()
    tilemap.set_draw_grid(not(tilemap.get_draw_grid()))
    if tilemap.get_draw_grid():
        grid_icon.set_down()
    else:
        grid_icon.set_up()
        
grid_icon = Icon(CL_Rect(CL_Point(p.inc(48), 2), CL_Size(32, 32)),
                 make_sprite("../data/images/icons24/grid.png"), "Some tooltip", button_panel);
grid_icon.set_callback(gui_toggle_grid)

layer_menu = Menu(CL_Point(32*11+2, 54), gui.get_component())

def set_tilemap_paint_tool():
    workspace.set_tool(tilemap_paint_tool.to_tool())
    paint.set_down()
    select.set_up()
    zoom.set_up()
    object.set_up()
#    supertux.show_tiles()

def set_tilemap_select_tool():
    workspace.set_tool(tilemap_select_tool.to_tool())
    paint.set_up()
    select.set_down()
    zoom.set_up()
    object.set_up()
#    supertux.show_none()
    
def set_zoom_tool():
    workspace.set_tool(zoom_tool.to_tool())
    paint.set_up()
    select.set_up()
    zoom.set_down()
    object.set_up()
#    supertux.show_none()
    
def set_objmap_select_tool():
    workspace.set_tool(objmap_select_tool.to_tool())
    paint.set_up()
    select.set_up()
    zoom.set_up()
    object.set_down()
#    supertux.show_objects()

toolbar = Panel(CL_Rect(CL_Point(0, 23+33), CL_Size(33, 32*4+2)), gui.get_component())

paint = Icon(CL_Rect(CL_Point(2, 32*0+2), CL_Size(32, 32)), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", toolbar);
paint.set_callback(set_tilemap_paint_tool)

select = Icon(CL_Rect(CL_Point(2, 32*1+2), CL_Size(32,32)), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", toolbar);
select.set_callback(set_tilemap_select_tool)

zoom = Icon(CL_Rect(CL_Point(2, 32*2+2), CL_Size(32,32)), make_sprite("../data/images/tools/stock-tool-zoom-22.png"), "Some tooltip", toolbar);
zoom.set_callback(set_zoom_tool)

object = Icon(CL_Rect(CL_Point(2, 32*3+2), CL_Size(32,32)), make_sprite("../data/images/tools/stock-tool-clone-22.png"), "Some tooltip", toolbar);
object.set_callback(set_objmap_select_tool)

# supertux = netPanzerGUI(load_supertux_tiles(), gui)
# supertux.tileselector.set_tileset(netpanzer.tileset)

level = None

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

def netpanzer_load_level(filename):
    level = Level(filename)
    level.activate(workspace)
    connect(level.editormap.sig_change(), on_map_change)
    
    if not(has_element(config.recent_files, filename)):
        config.recent_files.append(filename)
        recent_files_menu.add_item(mysprite, filename, lambda: netpanzer_load_level(filename))

    minimap.update_minimap()

def netpanzer_save_level(filename):
    workspace.get_map().get_metadata().save(filename)

recent_files_menu = Menu(CL_Point(32*2, 54), gui.get_component())
for filename in config.recent_files:
    recent_files_menu.add_item(mysprite, filename, lambda: netpanzer_load_level(filename))

def has_element(lst, el):
    for i in lst:
        if i == el:
            return True
    return False

menu = CL_Menu(gui.get_component())
menu.add_item("File/Open...", gui_level_load)
menu.add_item("File/Save...", gui_level_save)
menu.add_item("File/Save As...", gui_level_save_as)
menu.add_item("File/Quit",  gui.quit)

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

# minimap_panel = Panel(CL_Rect(CL_Point(0, 600-56), CL_Size(800-134, 56)), gui.get_component())
minimap = Minimap(editor_map, CL_Rect(CL_Point(3, 488+56 - 128-3), CL_Size(128, 128)), option_panel)

load_dialog = SimpleFileDialog("Load netPanzer Level", "Load", "Cancel", gui.get_component())
load_dialog.set_filename(config.datadir + "maps/")
save_dialog = SimpleFileDialog("Save netPanzer Level as...", "Save", "Cancel", gui.get_component())
save_dialog.set_filename(config.datadir + "maps/")

set_tilemap_paint_tool()

gui.run()

del config

flexlay.deinit()
print "deinit done"

# EOF #
