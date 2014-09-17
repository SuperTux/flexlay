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


import argparse

from flexlay import Flexlay

from .tileset import SuperTuxTileset
from .config import Config
from .gui import SuperTuxGUI


def main():
    parser = argparse.ArgumentParser(description="Flexlay - SuperTux Editor")
    parser.add_argument("LEVELFILE", action="store", type=str, nargs="?",
                        help=".stl file to load")
    parser.add_argument("-d", "--datadir", metavar="DIR", action="store", type=str,
                        help="SuperTux data directory directory")
    args = parser.parse_args()

    print("Datadir:", args.datadir)

    flexlay = Flexlay()

    config = Config.create("supertux-editor")
    if not config.datadir:
        if args.datadir:
            config.datadir = args.datadir
        else:
            raise RuntimeError("datadir missing, use --datadir DIR")

    tileset = SuperTuxTileset(32)
    tileset.load(config.datadir + "images/tiles.strf")
    # tileset.load(os.path.join(Config.current.datadir, "images/worldmap.strf"))
    tileset.create_ungrouped_tiles_group()

    tileset.create_ungrouped_tiles_group()

    gui = SuperTuxGUI(flexlay)
    if args.LEVELFILE is None:
        gui.new_level(100, 50)
    else:
        gui.load_level(args.LEVELFILE)

    # Init the GUI, so that button state is in sync with internal state
    gui.gui_toggle_minimap()
    gui.gui_toggle_minimap()
    gui.gui_show_interactive()
    gui.gui_show_current()
    gui.set_tilemap_paint_tool()

    gui.run()

    config.save()


# EOF #
