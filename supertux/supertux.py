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

from flexlay import Flexlay

from .tileset import SuperTuxTileset
from .config import Config
from .gui import SuperTuxGUI


def main():
    flexlay = Flexlay()

    config = Config.create("supertux-editor")
    if not config.datadir:
        config.datadir = os.path.expanduser("~/projects/supertux/trunk/supertux/data/")

    tileset = SuperTuxTileset(32)
    tileset.load(config.datadir + "images/tiles.strf")
    tileset.create_ungrouped_tiles_group()

    gui = SuperTuxGUI(flexlay)
    if not sys.argv[1:]:
        gui.new_level(100, 50)
    else:
        gui.load_level(sys.argv[1])

    # Init the GUI, so that button state is in sync with internal state
    gui.gui_toggle_minimap()
    gui.gui_toggle_minimap()
    gui.gui_show_interactive()
    gui.gui_show_current()
    gui.set_tilemap_paint_tool()

    gui.run()

    config.save()


# EOF #
