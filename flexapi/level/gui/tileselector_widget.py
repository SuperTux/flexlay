# Flexlay - A Generic 2D Game Editor
#
# ISC License
# Copyright (C) 2016 Karkus476 <karkus476@yahoo.com>
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR ON SEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

from PyQt4.QtGui import (QWidget, QGraphicsView, QVBoxLayout, QComboBox,
                         QGraphicsScene, QGraphicsPixmapItem, QGraphicsItemGroup)
from PyQt4.QtCore import Qt

from flexapi.level.level_tileset import LevelTileset
from flexapi.flexlay_error import FlexlayError
from flexapi.util import Signal


class TileView(QGraphicsView):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.left_click_signal = Signal()

    def mousePressEvent(self, event):
        if event.button() == 1:
            self.left_click_signal(event.x(), event.y())


class TileSelectorWidget(QWidget):
    def __init__(self, tileset=None, parent=None):
        super().__init__(parent)
        self.graphics_scene = QGraphicsScene(parent)

        self.current_scale = 1

        if not isinstance(tileset, LevelTileset):
            raise FlexlayError("tileset argument must be a LevelTileset object.")
        self.tileset = tileset
        self.current_group_name = ""

        self.combobox = QComboBox(parent)

        self.tile_view = TileView(self.graphics_scene)

        self.box = QWidget()
        self.layout = QVBoxLayout(self.box)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.combobox)
        self.layout.addWidget(self.tile_view)
        self.setLayout(self.layout)

        self.combobox.activated.connect(self.on_combobox_activated)

        self.tile_view.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)
        self.tile_view.setAlignment(Qt.AlignLeft | Qt.AlignTop)
        self.tile_view.show()

        if tileset is not None:
            self.set_tileset(tileset)

    def on_combobox_activated(self, i):
        self.set_tilegroup(self.combobox.itemText(i))

    def set_tilegroup(self, tilegroup_name):
        if tilegroup_name not in self.tileset.groups:
            raise FlexlayError("No such tilegroup named " + str(tilegroup_name))
        self.current_group_name = tilegroup_name
        tilegroup_combobox_index = self.combobox.findText(tilegroup_name)
        if self.combobox.currentIndex() != tilegroup_combobox_index:
            self.combobox.setCurrentIndex(tilegroup_combobox_index)
        self.repopulate_scene()

    def repopulate_scene(self):
        if self.tileset is None:
            raise FlexlayError("Cannot populate tile selector without tileset")

        self.graphics_scene.clear()

        item_group = QGraphicsItemGroup()

        # tile_size * number of columns
        max_column_width = self.tileset.tile_size * self.tileset.display_columns
        columns = self.tile_view.width() // max_column_width * self.tileset.display_columns
        column = 0
        row = 0
        for tile_id in self.tileset.groups[self.current_group_name]:
            image_resource = self.tileset.get_tile_image(tile_id)
            if image_resource is not None:
                graphics_item = QGraphicsPixmapItem(image_resource.get_pixmap())
                graphics_item.setPos(self.tileset.tile_size * column,
                                     self.tileset.tile_size * row)
                item_group.addToGroup(graphics_item)
            if column == columns:
                row += 1
                column = 0
            else:
                column += 1

        tiles_bounding_rect = item_group.boundingRect()
        self.graphics_scene.addItem(item_group)
        scale_factor = self.tile_view.width() / tiles_bounding_rect.width()
        self.tile_view.scale(1/self.current_scale, 1/self.current_scale)
        self.tile_view.scale(scale_factor, scale_factor)
        self.current_scale = scale_factor
        self.graphics_scene.setSceneRect(tiles_bounding_rect)

    def set_tileset(self, tileset):
        """Choose the tileset to display"""
        if not isinstance(tileset, LevelTileset):
            raise TypeError("tileset argument must be a LevelTileset object")
        for group in tileset.groups:
            self.combobox.addItem(group)
        self.tileset = tileset
        self.set_tilegroup("All Tiles")

    def resizeEvent(self, event):
        self.repopulate_scene()
