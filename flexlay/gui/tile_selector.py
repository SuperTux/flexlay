# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from PyQt4.QtCore import Qt
from PyQt4.QtGui import (QComboBox, QScrollArea, QWidget, QVBoxLayout)

from .tile_selector_widget import TileSelectorWidget


class TileSelector:
    def __init__(self):
        self.tiles = {}

        self.combobox = QComboBox()

        self.scroll_area = QScrollArea()
        self.widget = TileSelectorWidget(self.scroll_area.viewport())
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setWidget(self.widget)
        self.scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        self.box = QWidget()
        self.layout = QVBoxLayout(self.box)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.combobox)
        self.layout.addWidget(self.scroll_area)

        self.combobox.activated.connect(self.on_combobox_activated)

    def on_combobox_activated(self, idx):
        text = self.combobox.itemData(idx)
        tiles = self.tiles[text]
        self.widget.set_tiles(tiles)
        self.widget.repaint()
        self.scroll_area.update()

    def set_tileset(self, tileset):
        self.widget.set_tileset(tileset)

    def set_tiles_noname(self, tiles):
        self.widget.set_tiles(tiles)
        self.combobox.clear()
        self.combobox.hide()

    def add_tilegroup(self, name, tiles):
        self.tiles[name] = tiles
        self.combobox.addItem(name, name)
        self.combobox.show()

        # It's the first tilegroup, so show it
        if len(self.tiles) == 1:
            self.on_combobox_activated(0)

    def clear_tilegroups(self):
        self.tiles = {}
        self.combobox.clear()

    def get_tiles(self):
        return self.widget.get_tiles()

    def set_scale(self, scale):
        self.widget.set_scale(scale)

    def get_widget(self):
        return self.box

# EOF #
