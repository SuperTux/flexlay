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
import sys

from flexlay import Flexlay, Sprite, Tileset

from supertux import Config, Level
from .gui import SuperTuxGUI, supertux_load_level

datadir = None

flexlay = Flexlay()

mysprite = Sprite.from_file("../data/images/icons16/stock_paste-16.png")

config = Config()
if not datadir:
    datadir = os.path.expanduser("~/projects/supertux/trunk/supertux/data/")

tileset = Tileset(32)
tileset.load(datadir + "images/tiles.strf")
tileset.create_ungrouped_tiles_group()

gui = SuperTuxGUI(flexlay)

if sys.argv[1:] == []:
    Level(100, 50).activate(gui.workspace)
else:
    supertux_load_level(sys.argv[1])

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
