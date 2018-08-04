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


import csv
import random

from PyQt5.QtWidgets import QLabel, QListWidget, QHBoxLayout, QVBoxLayout, QSpinBox, QWidget

from flexlay.tile_brush import TileBrush
from flexlay.tool_context import ToolContext


class SmartTileMapping:
    id = -1
    top = []
    right = []
    bottom = []
    left = []


class SmartTile:

    id = 0
    name = "Undefined smart tile"
    width = 0
    height = 0
    start = 0
    reverse = False

    def __init__(self, id, name, width, height, max_width, max_height, start, reverse):
        self.id = id
        self.name = name
        self.width = width
        self.height = height
        self.max_width = max_width
        self.max_height = max_height
        self.min_width = width
        self.min_height = height
        self.start = start
        self.reverse = reverse

        self.top_left_ids = []
        self.top_ids = []
        self.top_right_ids = []
        self.right_ids = []
        self.bottom_right_ids = []
        self.bottom_ids = []
        self.bottom_left_ids = []
        self.left_ids = []
        self.other_ids = []

        self.has_mappings = False
        self.brush = None

    def as_brush(self):
        brush = TileBrush(self.width, self.height)
        brush.set_transparent()
        i = self.start
        if self.reverse:
            for x in range(0, self.width):
                for y in range(0, self.height):
                    brush.put(x, y, i)
                    i += 1
        else:
            for y in range(0, self.height):
                for x in range(0, self.width):
                    brush.put(x, y, i)
                    i += 1
        return brush

    def is_valid_id(self, id):
        return id >= self.start and id <= self.start + (self.width * self.height)

    """
    Return a tile that fits in the currently referenced gap.
    @param width: width of tile brush
    @param height: height of tile brush
    @param left: Tile ID at X - 1
    @param top: Tile ID at Y - 1
    """
    def get_fitting_tile(self, width, height, left, top, x, y, mappings=None):
        # Check mappings:
        if mappings is not None:
            matching_tiles = []
            right = 0 if x == width - 1 else - 1
            bottom = 0 if y == height - 1 else - 1
            for mapping in mappings:
                if left in mapping.left:
                    if right in mapping.right or right == -1:
                        if bottom in mapping.bottom or bottom == -1:
                            if top in mapping.top:
                                if self.is_valid_id(int(mapping.id)):
                                    matching_tiles.append(int(mapping.id))

            print("Found %s matching tiles for %s,%s,%s,%s" % (len(matching_tiles), top, right, bottom, left))
            if len(matching_tiles) > 0:
                return matching_tiles[random.randint(0, len(matching_tiles) - 1)]

        if left == 0 and top == 0:  # top left corner
            return self.top_left_ids[0]
        if left == width - 1 and top == 0:  # top right corner
            return self.top_right_ids[0]
        if left == 0 and top == height - 1:  # bottom left corner
            return self.bottom_left_ids[0]
        if left == width - 1 and top == height - 1:  # bottom right corner
            return self.bottom_right_ids[0]
        elif top == 0:  # top edge
            return self.top_ids[0]
        elif left == 0:  # left edge
            return self.left_ids[0]
        elif top == height - 1:  # bottom edge
            return self.bottom_ids[0]
        elif left == width - 1:  # right edge
            return self.right_ids[0]
        else:
            return self.other_ids[0]

    def as_smart_brush(self, width=None, height=None, mappings=None):
        brush = self.as_brush()
        if width is None and height is None:
            return brush

        if width and width < self.min_width:
            return brush
        if height and height < self.min_height:
            return brush

        if width is None:
            width = brush.width
        if height is None:
            height = brush.height

        if width > self.max_width and self.max_width != -1:
            width = self.max_width
        if height > self.max_height and self.max_height != -1:
            height = self.max_height

        brush.resize(width, height)

        for x in range(0, width):
            for y in range(0, height):
                if x > 0:
                    prev_x = brush.at(x - 1, y) or 0
                else:
                    prev_x = 0
                if y > 0:
                    prev_y = brush.at(x, y - 1) or 0
                else:
                    prev_y = 0
                tile = self.get_fitting_tile(width, height, prev_x, prev_y, x, y, mappings)
                brush.put(x, y, tile)

        return brush

    def set_mappings(self, mappings_string):
        mappings = mappings_string.split("|")
        if len(mappings) != 9:
            return

        self.has_mappings = True
        self.top_left_ids = [int(mappings[0])]
        self.top_ids = [int(mappings[1])]
        self.top_right_ids = [int(mappings[2])]
        self.right_ids = [int(mappings[3])]
        self.bottom_right_ids = [int(mappings[4])]
        self.bottom_ids = [int(mappings[5])]
        self.bottom_left_ids = [int(mappings[6])]
        self.left_ids = [int(mappings[7])]
        self.other_ids = [int(mappings[8])]


class SmartTileSelectorWidget(QWidget):
    def __init__(self, viewport):
        super().__init__()

        self.smart_tiles = []
        self.mappings = []
        self.load_tiles()
        self.layout = QVBoxLayout()

        self.list_widget = QListWidget(self)
        for tile in self.smart_tiles:
            self.list_widget.addItem(tile.name)

        self.width_label = QLabel("Width: ")
        self.width_input = QSpinBox(self)
        self.height_label = QLabel("Height: ")
        self.height_input = QSpinBox(self)
        self.width_box = QHBoxLayout()
        self.width_box.addWidget(self.width_label)
        self.width_box.addWidget(self.width_input)
        self.height_box = QHBoxLayout()
        self.height_box.addWidget(self.height_label)
        self.height_box.addWidget(self.height_input)
        self.layout.addWidget(self.list_widget)
        self.layout.addLayout(self.width_box)
        self.layout.addLayout(self.height_box)
        self.setLayout(self.layout)

        self.current = None
        self.list_widget.currentItemChanged.connect(self.item_changed)
        self.width_input.valueChanged.connect(self.set_brush)
        self.height_input.valueChanged.connect(self.set_brush)
        self.viewport = viewport

    def set_brush(self):
        current = self.get_current()
        # if current.has_mappings:
        self.brush = current.as_smart_brush(self.width_input.value(), self.height_input.value(), self.mappings)
        # else:
        #    self.brush = current.as_brush()

        if self.brush is not None:
            ToolContext.current.tile_brush = self.brush

    def set_tiles(self, tiles):
        pass

    def get_current(self):
        return self.smart_tiles[self.current]

    def set_current(self, index):
        self.current = index
        self.set_brush()

    def item_changed(self, curr, prev):
        item_index = self.list_widget.indexFromItem(curr).row()
        self.set_current(item_index)
        self.height_input.setValue(self.get_current().height)
        self.width_input.setValue(self.get_current().width)
        if self.get_current().has_mappings:
            self.height_input.setReadOnly(False)
            self.width_input.setReadOnly(False)

        else:
            self.height_input.setReadOnly(True)
            self.width_input.setReadOnly(True)

    def load_tiles(self):
        with open('data/supertux/smarttiles_mapping.csv', 'r') as mappings:
            reader = csv.reader(mappings)
            first_row = True
            for row in reader:
                if first_row:  # Ignore first heading row
                    first_row = False
                    continue
                mapping = SmartTileMapping()
                mapping.id = row[0]
                mapping.top = [int(val) for val in row[1].split("|")]
                mapping.right = [int(val) for val in row[2].split("|")]
                mapping.bottom = [int(val) for val in row[3].split("|")]
                mapping.left = [int(val) for val in row[4].split("|")]
                self.mappings.append(mapping)

        with open('data/supertux/smarttiles.csv', 'r') as csvfile:
            reader = csv.reader(csvfile)
            first_row = True
            for row in reader:
                if first_row:  # Ignore first heading row
                    first_row = False
                    continue

                id = row[0]
                name = row[1]
                width = int(row[2])
                height = int(row[3])
                start = int(row[4])
                reverse = row[5] if len(row) >= 6 and row[5] == "true" else False
                max_width = int(row[6]) if len(row) >= 7 else width
                max_height = int(row[7]) if len(row) >= 8 else height
                tile = SmartTile(id, name, width, height, max_width, max_height, start, reverse)
                mappings = row[8] if len(row) >= 9 else None
                if mappings is not None:
                    tile.set_mappings(mappings)

                self.smart_tiles.append(tile)

    #     self.index = 0
    #
    #     self.has_focus = False
    #     self.mouse_over_tile = -1
    #     self.region_select = False
    #     self.current_pos = Point()
    #     self.region_select_start = Point()
    #     self.mouse_pos = Point()
    #     self.scale = 1.0
    #     self.tileset = Tileset(32)
    #     self.tiles = []
    #
    #     self.setMouseTracking(True)
    #
    # def get_selection(self):
    #     selection = Rect(self.current_pos.x, self.current_pos.y,
    #                      self.region_select_start.x, self.region_select_start.y)
    #
    #     selection.normalize()
    #     selection.right += 1
    #     selection.bottom += 1
    #
    #     selection.left = min(max(0, selection.left), self.columns)
    #     selection.right = min(max(0, selection.right), self.columns)
    #
    #     selection.top = max(0, selection.top)
    #
    #     return selection
    #
    # def mousePressEvent(self, event):
    #     if event.button() == Qt.LeftButton:
    #         brush = TileBrush(1, 1)
    #
    #         brush.set_opaque()
    #         if 0 <= self.mouse_over_tile < len(self.tiles):
    #             brush.put(0, 0, self.tiles[self.mouse_over_tile])
    #         else:
    #             brush.put(0, 0, 0)
    #
    #         ToolContext.current.tile_brush = brush
    #
    #     elif event.button() == Qt.RightButton:
    #         self.region_select = True
    #         self.region_select_start = self.current_pos
    #         self.grabMouse()
    #
    #     self.repaint()
    #
    # def mouseReleaseEvent(self, event):
    #
    #     if event.button() == Qt.RightButton:
    #         self.releaseMouse()
    #         self.region_select = False
    #
    #         selection = self.get_selection()
    #         # selection.bottom = min(max(0, selection.right), self.columns)
    #
    #         brush = TileBrush(selection.width, selection.height)
    #         brush.set_transparent()
    #
    #         for y in range(0, selection.height):
    #             for x in range(0, selection.width):
    #                 tile = (selection.top + y) * self.columns + (selection.left + x)
    #
    #                 if 0 <= tile < len(self.tiles):
    #                     brush.put(x, y, self.tiles[tile])
    #                 else:
    #                     brush.put(x, y, 0)
    #
    #         ToolContext.current.tile_brush = brush
    #
    #     self.repaint()
    #
    # def mouseMoveEvent(self, event):
    #     pos = self.get_mouse_tile_pos(Point.from_qt(event.pos()))
    #     self.current_pos = pos
    #     self.mouse_over_tile = pos.y * self.columns + pos.x
    #     self.repaint()
    #
    # def get_mouse_tile_pos(self, mouse_pos):
    #     x = int(mouse_pos.x / self.cell_size)
    #     y = int(mouse_pos.y / self.cell_size)
    #
    #     if x >= self.columns:
    #         x = self.columns - 1
    #
    #     return Point(x, y)
    #
    # def paintEvent(self, event):
    #     painter = QPainter(self)
    #     gc = GraphicContext(painter)
    #
    #     brush = ToolContext.current.tile_brush
    #
    #     start_row = event.rect().top() // self.cell_size
    #     end_row = (event.rect().bottom() + self.cell_size - 1) // self.cell_size
    #     end_index = min(end_row * self.columns, len(self.tiles))
    #
    #     # Draw tiles
    #     for i in range(start_row * self.columns, end_index):
    #         x = i % self.columns
    #         y = i // self.columns
    #
    #         tile = self.tileset.create(self.tiles[i])
    #
    #         rect = Rect(Point(int(x * self.cell_size),
    #                           int(y * self.cell_size)),
    #                     Size(self.cell_size,
    #                          self.cell_size))
    #
    #         if tile:
    #             sprite = tile.get_sprite()
    #             sprite.set_scale(self.scale, self.scale)
    #             sprite.draw(int(x * self.cell_size),
    #                         int(y * self.cell_size), gc)
    #
    #             # Use grid in the tileselector
    #             gc.draw_rect(rect, Color(0, 0, 0, 128))
    #
    #         # mark the currently selected tile
    #         if brush.width == 1 and brush.height == 1 and brush.at(0, 0) == self.tiles[i]:
    #             gc.fill_rect(rect, Color(0, 0, 255, 100))
    #         elif self.mouse_over_tile == i and self.has_focus:
    #             gc.fill_rect(rect, Color(0, 0, 255, 20))
    #
    #     # draw rectangle selection
    #     if self.region_select:
    #         rect = self.get_selection()
    #
    #         rect.top *= self.cell_size
    #         rect.bottom *= self.cell_size
    #         rect.left *= self.cell_size
    #         rect.right *= self.cell_size
    #
    #         gc.fill_rect(rect, Color(0, 0, 255, 100))
    #
    # def enterEvent(self, event):
    #     self.has_focus = True
    #     self.repaint()
    #
    # def leaveEvent(self, event):
    #     self.has_focus = False
    #     self.repaint()
    #
    # def resizeEvent(self, event):
    #     self.update_minimum_size()
    #
    # def update_minimum_size(self):
    #     min_rows = (len(self.tiles) + self.columns - 1) / self.columns
    #     size = QSize(self.tileset.get_tile_size() * self.columns,
    #                  self.tileset.get_tile_size() * min_rows)
    #     self.setMinimumSize(size)
    #
    # @property
    # def columns(self):
    #     return int(self.viewport.width() / self.cell_size)
    #
    # def set_scale(self, s):
    #     self.scale = s
    #     self.repaint()
    #
    # def get_tiles(self):
    #     return self.tiles
    #
    # def set_tileset(self, tileset):
    #     self.tileset = tileset
    #     self.update_minimum_size()
    #
    # def set_tiles(self, tiles):
    #     self.tiles = tiles
    #     self.update_minimum_size()
    #
    # @property
    # def cell_size(self):
    #     return int(self.tileset.get_tile_size() * self.scale)

# EOF #
