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


from PyQt5.QtWidgets import (QComboBox, QScrollArea, QWidget, QVBoxLayout)

from .tile_selector_widget import TileSelectorWidget


class TileSelector:

    def __init__(self):

        self.tiles = {}

        self.combobox = QComboBox()

        self.scroll_area = QScrollArea()
        self.widget = TileSelectorWidget(self.scroll_area.viewport())
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setWidget(self.widget)

        self.box = QWidget()
        self.layout = QVBoxLayout(self.box)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.combobox)
        self.layout.addWidget(self.scroll_area)

        def on_activated(idx):
            text = self.combobox.itemData(idx)
            tiles = self.tiles[text]
            print("Setting tiles:", text, " - ", len(tiles))
            self.widget.set_tiles(tiles)
            self.scroll_area.update()

        self.combobox.activated.connect(on_activated)

    def set_tileset(self, tileset):
        self.widget.set_tileset(tileset)

    def set_tiles_noname(self, tiles):
        self.widget.set_tiles(tiles)
        self.combobox.clear()
        self.combobox.hide()

    def set_tiles(self, name, tiles):
        self.tiles[name] = tiles
        self.combobox.addItem(name, name)
        self.combobox.show()

    def get_tiles(self):
        return self.widget.get_tiles()

    def set_scale(self, scale):
        self.widget.set_scale(scale)

    def get_widget(self):
        return self.box


# EOF #
