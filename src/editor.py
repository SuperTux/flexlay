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

import sys
from flexlay import *
from supertux import *

flexlay = Flexlay()
flexlay.init()

editor = Editor()
gui = editor.get_gui_manager()

myrect = CL_Rect(CL_Point(0, 56), CL_Size(665, 488))
editor_map = EditorMapComponent(myrect, gui.get_component())
workspace  = Workspace(myrect.get_width(), myrect.get_height())
editor_map.set_workspace(workspace)

m = EditorMap()
workspace.set_map(m)

tileset = load_supertux_tiles()
tilemap = TilemapLayer(tileset, 200, 15)
m.add_layer(tilemap.to_layer())
    
TilemapLayer_set_current(tilemap)

def do_quit():
    print "---My Callback---"
    gui.quit()

def draw_something():
    print "Draw something"
    brush = TileBrush(2, 2)
    brush.set_opaque()
    _ = PaintCommand(tilemap, brush)
    _.add_point(CL_Point(1,1))
    _.add_point(CL_Point(2,2))
    _.add_point(CL_Point(3,3))
    _.add_point(CL_Point(4,4))
    m.execute(_.to_command())
    print "Draw something done"

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

load_icon    = Icon(CL_Point(32*0+2, 2), make_sprite("../data/images/icons24/stock_open.png"), "Some tooltip", willow);
save_icon    = Icon(CL_Point(32*1+2, 2), make_sprite("../data/images/icons24/stock_save.png"), "Some tooltip", willow);
save_as_icon = Icon(CL_Point(32*2+2, 2), make_sprite("../data/images/icons24/stock_save_as.png"), "Some tooltip", willow);

copy_icon    = Icon(CL_Point(32*3.1+2, 2), make_sprite("../data/images/icons24/stock_copy.png"), "Some tooltip", willow);
paste_icon   = Icon(CL_Point(32*4.1+2, 2), make_sprite("../data/images/icons24/stock_paste.png"), "Some tooltip", willow);

undo_icon = Icon(CL_Point(32*5.1+2, 2), make_sprite("../data/images/icons24/stock_undo.png"), "Some tooltip", willow);
redo_icon = Icon(CL_Point(32*6.1+2, 2), make_sprite("../data/images/icons24/stock_redo.png"), "Some tooltip", willow);

undo_icon.set_callback(m.undo)
redo_icon.set_callback(m.redo)

toolbar = Panel(CL_Rect(CL_Point(0, 23+33), CL_Size(33, 256)), gui.get_component())

select = Icon(CL_Point(2, 32*0+2), make_sprite("../data/images/tools/stock-tool-rect-select-22.png"), "Some tooltip", toolbar);
erase  = Icon(CL_Point(2, 32+1+2), make_sprite("../data/images/tools/stock-tool-eraser-22.png"), "Some tooltip", toolbar);
move   = Icon(CL_Point(2, 32*2+2), make_sprite("../data/images/tools/stock-tool-move-22.png"), "Some tooltip", toolbar);
paint  = Icon(CL_Point(2, 32*3+2), make_sprite("../data/images/tools/stock-tool-pencil-22.png"), "Some tooltip", toolbar);

supertux = SuperTuxGUI(tileset, gui)

def block():
    def CL_Menu_add_item(self, name, func):
        item = self.create_item(name)
        connect(item.sig_clicked(), func)
    CL_Menu.add_item = CL_Menu_add_item
block()

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

menu = CL_Menu(gui.get_component())
a = menu.add_item("File/Open...", menu_file_open)
a = menu.add_item("File/Save...", menu_file_save)
a = menu.add_item("File/Save As...", menu_file_save_as)
a = menu.add_item("File/Quit",  do_quit)

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")

def Menu_add_item(self, sprite, text, func):
    i = self.__add_item(mysprite, text)
    if func != None:
        connect(self.sig_clicked(i), func)
Menu.__add_item = Menu.add_item
Menu.add_item = Menu_add_item
del Menu_add_item

mymenu = Menu(CL_Point(100, 100), gui.get_component())
mymenu.add_item(mysprite, "Foobar aeuaeu", None)
mymenu.add_item(mysprite, "blub", do_something)
mymenu.add_item(mysprite, "bla", None)
mymenu.add_seperator()
mymenu.add_item(mysprite, "Foobar", None)
mymenu.add_item(mysprite, "blubaoeuau aueau aeu", None)
mymenu.add_item(mysprite, "bla", None)

def show_menu():
    mymenu.run()

copy_icon.set_callback(show_menu)
    
# _button = CL_Button(CL_Rect(100, 100, 200, 125), "Hello World", gui.get_component())
# connect(_button.sig_clicked(), show_menu)

minimap_panel = Panel(CL_Rect(CL_Point(0, 600-56), CL_Size(800-134, 56)), gui.get_component())
minimap = Minimap(editor_map, CL_Rect(CL_Point(3, 3), CL_Size(794-134, 50)), minimap_panel)

gui.run()

flexlay.deinit()
print "deinit done"

# EOF #
