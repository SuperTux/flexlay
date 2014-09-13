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


config_file = File.expand_path("~/.flexlay/supertux-worldmap.rb")

BACKGROUND_LAYER = 1
INTERACTIVE_LAYER = 2
FOREGROUND_LAYER = 3

flexlay = Flexlay()
width = 1024
height = 768
flexlay.init("SuperTux Worldmap Editor", width, height)

# Tools
tilemap_paint_tool = TileMapPaintTool()
tilemap_select_tool = TileMapSelectTool()
zoom_tool = ZoomTool()
objmap_select_tool = ObjMapSelectTool()
# sketch_stroke_tool  = SketchStrokeTool()

config = Config()
if not datadir:
    datadir = File.expand_path("~/projects/supertux/data/") + "/"

tileset = Tileset(32)
tileset.load(datadir + "images/worldmap.strf")
tileset.create_ungrouped_tiles_group()

mysprite = make_sprite("../data/images/icons16/stock_paste-16.png")
gui = SuperTuxGUI(width, height)

if not recent_files:
    recent_files = []

for filename in recent_files:
    gui.recent_files_menu.add_item(filename, lambda filename=filename: supertux_load_level(filename))

if ARGV == []:
    WorldMap(70, 50).activate(gui.workspace)
    use_worldmap = True
else:
    supertux_load_worldmap(ARGV[0])

# Init the GUI, so that button state is in sync with internal state
gui.gui_toggle_minimap()
gui.gui_toggle_minimap()
# gui.gui_show_interactive()
gui.gui_show_current()
gui.set_tilemap_paint_tool()

gui.run()

config.save(config_file)

# EOF #
