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

from PyQt4.QtGui import QApplication

from flexapi.tabs import EditorTab
from flexapi.elements import EditorElement
from flexapi.level.gui.tileselector_widget import TileSelectorWidget
from flexapi.level.level_tileset import LevelTileset
from flexapi import FlexlayError, FlexlayEditor
from flexapi.resources import ImageFileResource,  PathResource, FLEXLAY_DATA_DIR


class TilesetTab(EditorTab):
    def __init__(self, item, parent):
        super().__init__(item)
        if not isinstance(item, LevelTileset):
            raise FlexlayError("item must be LevelTileset")
        self.widget = TileSelectorWidget(tileset=item, parent=parent)

    @classmethod
    def can_edit(Tab, item):
        super().can_edit(item)
        if isinstance(item, LevelTileset):
            return 8
        return 0

    def get_value(self):
        return self.widget.toPlainText()

    def get_widget(self):
        return self.widget


if __name__ == "__main__":
    app = QApplication([])

    editor = FlexlayEditor("tileset_editor", 0)

    editor_map_element = EditorElement()
    editor.add_element("editor_map", editor_map_element)
    editor_map_element.add_tab_type(TilesetTab)
    ifr1 = ImageFileResource(FLEXLAY_DATA_DIR + PathResource("images/sheet1.png"))
    tileset = LevelTileset(tile_size=32)
    tileset.add_tilesheet(list(range(1, 65)), ifr1.get_image_resource())
    tileset.groups["Land"] = [5, 6, 7, 14]

    editor_map_element.edit("Test Tileset", tileset)

    editor.run()
