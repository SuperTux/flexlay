# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


import os

from supertux import Config


BACKGROUND_LAYER = 1
INTERACTIVE_LAYER = 2
FOREGROUND_LAYER = 3

flexlay = Flexlay()

# Tools
tilemap_paint_tool = TileMapPaintTool()
tilemap_select_tool = TileMapSelectTool()
zoom_tool = ZoomTool()
zoom2_tool = Zoom2Tool()
workspace_move_tool = WorkspaceMoveTool()
objmap_select_tool = ObjMapSelectTool()
# sketch_stroke_tool = SketchStrokeTool()

mysprite = Sprite.from_file("../data/images/icons16/stock_paste-16.png")

config = Config()
if not datadir:
    datadir = File.expand_path("~/projects/supertux/trunk/supertux/data/") + "/"

tileset = Tileset(32)
tileset.load(datadir + "images/tiles.strf")
tileset.create_ungrouped_tiles_group()

if not recent_files:
    recent_files = []

gui = SuperTuxGUI()
for filename in recent_files:
    gui.recent_files_menu.add_item(filename, lambda filename=filename: supertux_load_level(filename))

if ARGV == []:
    Level(100, 50).activate(gui.workspace)
else:
    supertux_load_level(ARGV[0])

# Init the GUI, so that button state is in sync with internal state
gui.gui_toggle_minimap()
gui.gui_toggle_minimap()
gui.gui_show_interactive()
gui.gui_show_current()
gui.set_tilemap_paint_tool()

if os.path.isdir(datadir):
    dialog = gui.gui.create_generic_dialog("Specify the SuperTux data directory and restart")
    dialog.add_label("You need to specify the datadir where SuperTux is located")
    dialog.add_string("Datadir:", datadir)

    def on_callback(datadir):
        datadir = datadir

    dialog.set_ok_callback(on_callback)

gui.run()

config.save()

# EOF #
