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
import os
import logging

from PyQt5.QtCore import QByteArray
from PyQt5.QtWidgets import QMessageBox, QFileDialog

from flexlay import Flexlay, Config
from flexlay.util.sexpr import SExprParseError
from .gui import SuperTuxGUI
from .tileset import SuperTuxTileset


def main():
    # Parse Arguments
    parser = argparse.ArgumentParser(description="Flexlay - SuperTux Editor")
    parser.add_argument("LEVELFILE", action="store", type=str, nargs="?",
                        help=".stl file to load")
    parser.add_argument("-d", "--datadir", metavar="DIR", action="store", type=str,
                        help="SuperTux data directory directory")
    parser.add_argument("-b", "--binary", metavar="BIN", action="store", type=str,
                        help="SuperTux binary path")
    args = parser.parse_args()

    # Create flexlay instance
    flexlay = Flexlay("SuperTux")

    # Load data directory path from config file, --datadir argument or open directory dialog
    config = Config.create("supertux-editor")
    config.create_attribute("datadir", None)
    config.create_attribute("binary", None)
    config.create_attribute("name", "Anonymous")
    # config.create_attribute()
    config.load()
    if args.datadir is not None:
        config.datadir = args.datadir
    if not config.datadir or not os.path.isdir(config.datadir):
        QMessageBox.warning(None, "No Data Directory Found",
                            "Press OK to select your Supertux directory")
        config.datadir = QFileDialog.getExistingDirectory(None, "Open Data Directory")
        if not config.datadir:
            raise RuntimeError("datadir missing, use --datadir DIR")

    logging.info("Datadir:" + config.datadir)

    # Load supertux binary path from config file, --binary argument or open file dialog
    if args.binary and os.path.isfile(args.binary):
        config.binary = args.binary
    if not config.binary or not os.path.isfile(config.binary):
        QMessageBox.warning(None, "No Supertux Binary Found",
                            "Press OK to select your Supertux binary")
        config.binary = QFileDialog.getOpenFileName(None, "Open Supertux Binary")
        if not config.binary:
            raise RuntimeError("binary path missing, use --binary BIN")

    logging.info("Binary path:" + config.binary)

    # Load tileset
    tileset = SuperTuxTileset(32)
    tileset.load(os.path.join(config.datadir, "images/tiles.strf"))
    tileset.create_ungrouped_tiles_group()

    gui = SuperTuxGUI(flexlay)
    if args.LEVELFILE is not None:
        gui.load_level(args.LEVELFILE)
    elif len(config.recent_files) > 0:
        # Check that at least one file is loaded
        any_valid = False
        for recent_file_path in reversed(config.recent_files):
            try:
                gui.load_level(recent_file_path)
            except: # TODO: Except only reasonable errors.
                config.recent_files.remove(recent_file_path)
                continue
            else:
                any_valid = True
                break
        if not any_valid:
            logging.info("All recent files broken. Creating a new level...")
            gui.gui_level_new()
    else:
        gui.gui_level_new()

    # Init the GUI, so that button state is in sync with internal state
    gui.gui_toggle_minimap()
    gui.gui_toggle_minimap()
    gui.set_tilemap_paint_tool()

    if config.geometry:
        if not gui.gui.window.restoreGeometry(QByteArray.fromBase64(config.geometry.encode("ascii"))):
            logging.error("error: failed to restore window geometry")
    if config.window_state:
        if not gui.gui.window.restoreState(QByteArray.fromBase64(config.window_state.encode("ascii"))):
            logging.error("error: failed to restore window state")

    gui.run()

    config.save()

# EOF #
