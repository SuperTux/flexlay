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


from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import (QComboBox, QScrollArea, QTabWidget, QWidget, QVBoxLayout)

from flexlay.gui.smart_tile_selector_widget import SmartTileSelectorWidget
from flexlay.gui.tile_selector_widget import TileSelectorWidget


class TileSelector:

    def __init__(self) -> None:
        self.tiles = {}

        self.combobox = QComboBox()

        self.scroll_area = QScrollArea()
        self.widget = TileSelectorWidget(self.scroll_area.viewport())
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setWidget(self.widget)
        self.scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        # self.second_tab_scroll_area = QScrollArea()
        self.smart_widget = SmartTileSelectorWidget(self.scroll_area.viewport())
        # self.second_tab_scroll_area.setWidgetResizable(True)
        # self.second_tab_scroll_area.setWidget(self.smart_widget)
        # self.second_tab_scroll_area.setVerticalScrollBarPolicy(Qt.ScrollBarAlwaysOn)

        self.box = QTabWidget()
        self.layout = QVBoxLayout(self.box)
        self.layout.setContentsMargins(0, 0, 0, 0)

        self.first_tab = QWidget()
        self.first_tab.layout = QVBoxLayout(self.first_tab)
        self.first_tab.layout.setContentsMargins(0, 0, 0, 0)
        self.first_tab.layout.addWidget(self.combobox)
        self.first_tab.layout.addWidget(self.scroll_area)

        self.second_tab = QWidget()
        self.second_tab.layout = QVBoxLayout(self.second_tab)
        self.second_tab.layout.setContentsMargins(0, 0, 0, 0)
        self.second_tab.layout.addWidget(self.smart_widget)

        self.box.addTab(self.first_tab, "Tile Selection")

        # SmartTiles is a working title, feel free to change.
        # TM symbol clearly meant as a joke
        self.box.addTab(self.second_tab, "SmartTiles™")

        self.combobox.activated.connect(self.on_combobox_activated)

    def on_combobox_activated(self, idx):
        text = self.combobox.itemData(idx)
        tiles = self.tiles[text]
        self.widget.set_tiles(tiles)
        self.widget.repaint()
        self.scroll_area.update()

        # self.smart_widget.set_tiles(tiles)
        # self.smart_widget.repaint()
        # self.smart_widget.update()
        # self.second_tab_scroll_area.update()

    def set_tileset(self, tileset):
        self.widget.set_tileset(tileset)

    def get_tileset(self):
        return self.widget.tileset

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

    def clear_tilegroups(self) -> None:
        self.tiles = {}
        self.combobox.clear()

    def get_tiles(self):
        return self.widget.get_tiles()

    def set_scale(self, scale):
        self.widget.set_scale(scale)

    def get_widget(self):
        return self.box


# EOF #
