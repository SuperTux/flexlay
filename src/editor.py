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

import os
import sys
from flexlay import *

flexlay = Flexlay()
flexlay.init()

from supertux import *

editor = Editor()
gui = editor.get_gui_manager()

def Editor_undo(self):
    workspace.get_map().undo()
def Editor_redo(self):
    workspace.get_map().redo()
Editor.undo = Editor_undo
Editor.redo = Editor_redo
del Editor_redo
del Editor_undo

EditorMap.__get_metadata = EditorMap.get_metadata
def EditorMap_get_metadata(self):
    return get_python_object(self.__get_metadata())
EditorMap.get_metadata = EditorMap_get_metadata
del EditorMap_get_metadata

# (define (simple-file-dialog title filename func)
#   (let ((window (gui-create-window 200 100 460 125 title)))
#     (gui-push-component (gui-window-get-client-area window))
#     (gui-create-label 10 10 "Filename: ")
#     (let ((ok       (gui-create-button 390 60 50 25 "Ok"))
#           (cancel   (gui-create-button 330 60 50 25 "Cancel"))
#           (filename (gui-create-inputbox 10 30 435 30 filename))
#           ;;(browse   (gui-create-button 190 30 50 20 "Browse..."))
#           )

#       (gui-component-on-click ok 
#                               (lambda ()   
#                                 (func (gui-inputbox-get-text filename))
#                                 (gui-hide-component window)))

#       (gui-component-on-click cancel
#                               (lambda () 
#                                 (gui-hide-component window)))

#       ;;      (gui-component-on-click browse
#       ;;                              (lambda ()
#       ;;                                (gui-file-dialog (gui-inputbox-get-text filename)
#       ;;                                                (lambda (filename)
#       ;;                                                   (gui-inputbox-set-text filename)))))

#       (gui-pop-component)
#       window)))
   

myrect = CL_Rect(CL_Point(0, 56), CL_Size(665, 488))
editor_map = EditorMapComponent(myrect, gui.get_component())
workspace  = Workspace(myrect.get_width(), myrect.get_height())
editor_map.set_workspace(workspace)

# Tools
tilemap_paint_tool  = TileMapPaintTool()
tilemap_select_tool = TileMapSelectTool()
zoom_tool           = ZoomTool()
objmap_select_tool  = ObjMapSelectTool()

workspace.set_tool(tilemap_paint_tool.to_tool());

startlevel = SuperTuxLevel()
startlevel.activate(workspace)

def do_quit():
    print "---My Callback---"
    gui.quit()

def draw_something():
    brush = TileBrush(2, 2)
    brush.set_opaque()
    _ = PaintCommand(tilemap, brush)
    _.add_point(CL_Point(1,1))
    _.add_point(CL_Point(2,2))
    _.add_point(CL_Point(3,3))
    _.add_point(CL_Point(4,4))
    workspace.get_map().execute(_.to_command())

def block():
    window = Window(CL_Rect(50, 50, 450, 400), "My Window", gui.get_component())
    
    gui.push_component(window.get_client_area())
    dirview = DirectoryView(CL_Rect(CL_Point(3, 40), CL_Size(300, 200)), gui.get_component())
    dirview.set_directory("/");

    scrollbar = Scrollbar(CL_Rect(CL_Point(370, 5), CL_Size(12, 300)), Scrollbar.VERTICAL, gui.get_component())
    scrollbar.set_range(50, 150)
    scrollbar.set_pagesize(10)
    scrollbar.set_pos(100)

    load_icon    = Icon(CL_Point(34*0+2, 2), make_sprite("../data/images/icons24/stock_open.png"), "Some tooltip", gui.get_component());
    save_icon    = Icon(CL_Point(34*1+2, 2), make_sprite("../data/images/icons24/stock_save.png"), "Some tooltip", gui.get_component());
    save_as_icon = Icon(CL_Point(34*2+2, 2), make_sprite("../data/images/icons24/stock_save_as.png"), "Some tooltip", gui.get_component());
    
    copy_icon    = Icon(CL_Point(34*3.1+2, 2), make_sprite("../data/images/icons24/stock_copy.png"), "Some tooltip", gui.get_component());
    paste_icon   = Icon(CL_Point(34*4.1+2, 2), make_sprite("../data/images/icons24/stock_paste.png"), "Some tooltip", gui.get_component());
    
    def foo():
        print "Button pressed"

    connect(load_icon.sig_clicked(), foo)

    gui.pop_component()

willow = Panel(CL_Rect(CL_Point(0, 23), CL_Size(800, 33)), gui.get_component())

def Icon_set_callback(self, func):
    connect(self.sig_clicked(), func)
Icon.set_callback = Icon_set_callback
del Icon_set_callback

def do_something():
    print "do_something"

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

load_icon    = Icon(CL_Point(32*0+2, 2), make_sprite("../data/images/icons24/stock_open.png"), "Some tooltip", willow);
save_icon    = Icon(CL_Point(32*1+2, 2), make_sprite("../data/images/icons24/stock_save.png"), "Some tooltip", willow);
save_as_icon = Icon(CL_Point(32*2+2, 2), make_sprite("../data/images/icons24/stock_save_as.png"), "Some tooltip", willow);

load_icon.set_callback(gui_level_load)
save_icon.set_callback(gui_level_save)
save_as_icon.set_callback(gui_level_save_as)

copy_icon    = Icon(CL_Point(32*3.1+2, 2), make_sprite("../data/images/icons24/stock_copy.png"), "Some tooltip", willow);
paste_icon   = Icon(CL_Point(32*4.1+2, 2), make_sprite("../data/images/icons24/stock_paste.png"), "Some tooltip", willow);

undo_icon = Icon(CL_Point(32*5.1+2, 2), make_sprite("../data/images/icons24/stock_undo.png"), "Some tooltip", willow);
redo_icon = Icon(CL_Point(32*6.1+2, 2), make_sprite("../data/images/icons24/stock_redo.png"), "Some tooltip", willow);

undo_icon.set_callback(editor.undo)
redo_icon.set_callback(editor.redo)

undo_icon.disable()
redo_icon.disable()

foreground_icon  = Icon(CL_Point(32*8+2, 2), make_sprite("../data/images/icons24/foreground.png"), "Some tooltip", willow);
interactive_icon = Icon(CL_Point(32*9+2, 2), make_sprite("../data/images/icons24/interactive.png"), "Some tooltip", willow);
background_icon  = Icon(CL_Point(32*10+2, 2), make_sprite("../data/images/icons24/background.png"), "Some tooltip", willow);
eye_icon = Icon(CL_Point(32*11+2, 2), make_sprite("../data/images/icons24/eye.png"), "Some tooltip", willow);

layer_menu = Menu(CL_Point(32*11+2, 54), gui.get_component())

def on_map_change():
    if (workspace.get_map().undo_stack_size() > 0):
        undo_icon.enable()
    else:
        undo_icon.disable()

    if (workspace.get_map().redo_stack_size() > 0):
        redo_icon.enable()
    else:
        redo_icon.disable()        

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

paint  = Icon(CL_Point(2, 32*0+2), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", toolbar);
paint.set_callback(set_tilemap_paint_tool)

select = Icon(CL_Point(2, 32*1+2), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", toolbar);
select.set_callback(set_tilemap_select_tool)

zoom   = Icon(CL_Point(2, 32*2+2), make_sprite("../data/images/tools/stock-tool-zoom-22.png"), "Some tooltip", toolbar);
zoom.set_callback(set_zoom_tool)

object = Icon(CL_Point(2, 32*3+2), make_sprite("../data/images/tools/stock-tool-clone-22.png"), "Some tooltip", toolbar);
object.set_callback(set_objmap_select_tool)

# erase  = Icon(CL_Point(2, 32+1+2), make_sprite("../data/images/tools/stock-tool-eraser-22.png"), "Some tooltip", toolbar);
# move   = Icon(CL_Point(2, 32*2+2), make_sprite("../data/images/tools/stock-tool-move-22.png"), "Some tooltip", toolbar);

def menu_show_foreground():
    display_properties.layer = SuperTuxLevel.FOREGROUND
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().foreground)
    foreground_icon.set_down()
    interactive_icon.set_up()
    background_icon.set_up()
    minimap.update_minimap()

def menu_show_background():
    display_properties.layer = SuperTuxLevel.BACKGROUND
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().background)
    foreground_icon.set_up()
    interactive_icon.set_up()
    background_icon.set_down()
    minimap.update_minimap()

def menu_show_interactive():
    display_properties.layer = SuperTuxLevel.INTERACTIVE
    display_properties.set(workspace.get_map().get_metadata())
    TilemapLayer_set_current(workspace.get_map().get_metadata().interactive)
    foreground_icon.set_up()
    interactive_icon.set_down()
    background_icon.set_up()
    minimap.update_minimap()

def menu_show_all():
    display_properties.show_all = True
    display_properties.set(workspace.get_map().get_metadata())

def menu_show_only_current():
    display_properties.show_all = False
    display_properties.set(workspace.get_map().get_metadata())

foreground_icon.set_callback(menu_show_foreground)
interactive_icon.set_callback(menu_show_interactive)
background_icon.set_callback(menu_show_background)
eye_icon.set_callback(layer_menu.run)

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

def block():
    def CL_Menu_add_item(self, name, func):
        item = self.create_item(name)
        connect(item.sig_clicked(), func)
    CL_Menu.add_item = CL_Menu_add_item
block()

def Menu_add_item(self, sprite, text, func):
    i = self.__add_item(sprite, text)
    if func != None:
        connect(self.sig_clicked(i), func)
Menu.__add_item = Menu.add_item
Menu.add_item = Menu_add_item
del Menu_add_item

layer_menu.add_item(mysprite, "Show all", menu_show_all)
layer_menu.add_item(mysprite, "Show only current", menu_show_only_current)

supertux = SuperTuxGUI(load_supertux_tiles(), gui)

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
    
def supertux_load_level(filename):
    print "Loading: ", filename
    level = SuperTuxLevel(filename)
    level.activate(workspace)
    connect(level.editormap.sig_change(), on_map_change)

menu = CL_Menu(gui.get_component())
menu.add_item("File/Open...", gui_level_load)
menu.add_item("File/Save...", gui_level_save)
# menu.add_item("File/Save Commands...", menu_file_save_commands)
menu.add_item("File/Save As...", gui_level_save_as)
menu.add_item("File/Quit",  do_quit)

display_properties = DisplayProperties()

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

mymenu = Menu(CL_Point(134, 54), gui.get_component())
mymenu.add_item(mysprite, "Foobar aeuaeu", None)
mymenu.add_item(mysprite, "blub", do_something)
mymenu.add_item(mysprite, "bla", None)
mymenu.add_seperator()
mymenu.add_item(mysprite, "Foobar", None)
mymenu.add_item(mysprite, "blubaoeuau aueau aeu", None)
mymenu.add_item(mysprite, "bla", None)

def show_menu():
    mymenu.run()

paste_icon.set_callback(show_menu)

minimap_panel = Panel(CL_Rect(CL_Point(0, 600-56), CL_Size(800-134, 56)), gui.get_component())
minimap = Minimap(editor_map, CL_Rect(CL_Point(3, 3), CL_Size(794-134, 50)), minimap_panel)

class FileDialog:
    window   = None
    inputbox = None
    ok_button     = None
    cancel_button = None
    callback = None

    def __init__(self, title, g):
        self.window   = Window(CL_Rect(CL_Point(120, 200), CL_Size(560, 100)), title, g)
        self.inputbox = CL_InputBox(CL_Rect(CL_Point(10, 10), CL_Size(530, 25)),
                                    self.window.get_client_area())
        self.ok_button     = CL_Button(CL_Rect(CL_Point(490, 35), CL_Size(50, 25)), "Ok",
                                       self.window.get_client_area())
        self.cancel_button = CL_Button(CL_Rect(CL_Point(430, 35), CL_Size(50, 25)), "Cancel",
                                       self.window.get_client_area())
        self.window.hide()

    def set_filename(self, filename):
        self.inputbox.set_text(filename)

    def get_filename(self):
        return self.inputbox.get_text()
        
    def run(self, func):
        connect(self.ok_button.sig_clicked(), self.on_ok)
        connect(self.cancel_button.sig_clicked(), self.on_cancel)
        self.callback = func
        self.window.show()
        
    def on_ok(self):
        self.window.hide();
        self.callback(self.inputbox.get_text())

    def on_cancel(self):
        self.window.hide();
        
def do_something_with_file(filename):
    print "DoSomething: ", filename

load_dialog = FileDialog("Load SuperTux Level", gui.get_component())
load_dialog.set_filename(supertux_datadir + "levels/")
save_dialog = FileDialog("Save SuperTux Level as...", gui.get_component())
save_dialog.set_filename(supertux_datadir + "levels/")

gui.run()

flexlay.deinit()
print "deinit done"

# EOF #
