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
import os
import sys
import code
from optparse import OptionParser
       
# Begin: Main loop
execfile("SuperTux/args.py")
(options, args) = parser.parse_args()

print "SUperTUX"
flexlay = Flexlay()
flexlay.init()
print "..SUperTUX"

execfile("SuperTux/__init__.py")

editor = Editor()
gui = editor.get_gui_manager()

myrect     = CL_Rect(CL_Point(0, 56), CL_Size(665, 488+56))
editor_map = EditorMapComponent(myrect, gui.get_component())
workspace  = Workspace(myrect.get_width(), myrect.get_height())
editor_map.set_workspace(workspace)

# Tools
tilemap_paint_tool  = TileMapPaintTool()
tilemap_select_tool = TileMapSelectTool()
zoom_tool           = ZoomTool()
objmap_select_tool  = ObjMapSelectTool()

workspace.set_tool(tilemap_paint_tool.to_tool());

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

recent_files_menu = Menu(CL_Point(32*2, 54), gui.get_component())
for filename in config.recent_files:
    print filename
    fun = lambda: sys.stdout.write("bla bla: %s\n" % filename)
    fun()
    recent_files_menu.add_item(mysprite, filename, fun)
    #supertux_load_level(filename))

minimap = Minimap(editor_map, CL_Rect(CL_Point(3, 488+3-14), CL_Size(794-134-16, 50)), editor_map)

if args == []:
    startlevel = Level(100, 50)
    # startlevel = netpanzer.Level(256, 256)
    startlevel.activate(workspace)
    connect(startlevel.editormap.sig_change(), on_map_change)
else:
    supertux_load_level(args[0])

button_panel = Panel(CL_Rect(CL_Point(0, 23), CL_Size(800, 33)), gui.get_component())

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

foreground_icon.set_callback(gui_show_foreground)
interactive_icon.set_callback(gui_show_interactive)
background_icon.set_callback(gui_show_background)
eye_icon.set_callback(layer_menu.run)

layer_menu.add_item(mysprite, "Show all", gui_show_all)
layer_menu.add_item(mysprite, "Show current", gui_show_current)
layer_menu.add_item(mysprite, "Show only current", gui_show_only_current)

supertux = SuperTuxGUI(tileset, gui)

level = None

menu = CL_Menu(gui.get_component())
menu.add_item("File/Open...", gui_level_load)
menu.add_item("File/Save...", gui_level_save)
# menu.add_item("File/Save Commands...", menu_file_save_commands)
menu.add_item("File/Save As...", gui_level_save_as)
menu.add_item("File/Quit",  gui.quit)

menu.add_item("Edit/Resize", lambda: gui_resize_level())
menu.add_item("Edit/Resize to selection", lambda: gui_resize_level_to_selection())
menu.add_item("Edit/Debug Shell", lambda: run_python())
    
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

class PathNode:
    node = None
    
    def __init__(self, node):
        self.node = node

def insert_path_node(x,y):
    print "Insert path Node"
    m = workspace.get_map().get_metadata()
    pathnode = ObjMapPathNode(editor_map.screen2world(CL_Point(x, y)),
                              make_metadata("PathNode"))
    pathnode.to_object().set_metadata(make_metadata(PathNode(pathnode)))
    m.objects.add_object(pathnode.to_object())

def connect_path_nodes():
    print "Connecting path nodes"
    pathnodes = []
    for i in objmap_select_tool.get_selection():
        obj = get_python_object(i.get_metadata())
        if obj.__class__ == PathNode:
            pathnodes.append(obj.node)

    last = None
    for i in pathnodes:
        if last != None:
            last.connect(i)
        last = i
            
connect_v2(editor_map.sig_on_key("f1"), lambda x, y: gui_toggle_minimap())
connect_v2(editor_map.sig_on_key("m"),  lambda x, y: gui_toggle_minimap())
connect_v2(editor_map.sig_on_key("g"),  lambda x, y: gui_toggle_grid())
connect_v2(editor_map.sig_on_key("4"),  lambda x, y: gui_toggle_display_props())
connect_v2(editor_map.sig_on_key("3"),  lambda x, y: gui_show_foreground())
connect_v2(editor_map.sig_on_key("2"),  lambda x, y: gui_show_interactive())
connect_v2(editor_map.sig_on_key("1"),  lambda x, y: gui_show_background())

connect_v2(editor_map.sig_on_key("5"),  lambda x, y: editor_map.zoom_in(CL_Point(x, y)))
connect_v2(editor_map.sig_on_key("6"),  lambda x, y: editor_map.zoom_out(CL_Point(x, y)))

connect_v2(editor_map.sig_on_key("i"),  lambda x, y: insert_path_node(x,y))
connect_v2(editor_map.sig_on_key("c"),  lambda x, y: connect_path_nodes())

connect_v2(editor_map.sig_on_key("7"),  lambda x, y: workspace.get_map().get_metadata().activate_sector("main", workspace))
connect_v2(editor_map.sig_on_key("8"),  lambda x, y: workspace.get_map().get_metadata().activate_sector("another_world", workspace))

gui.run()

del config

flexlay.deinit()
print "deinit done"
    
### End: 'Main Loop'

# EOF #
