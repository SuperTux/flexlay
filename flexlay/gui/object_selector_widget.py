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


from typing import Optional

import pickle

from PyQt5.QtCore import Qt, QSize, QPoint, QByteArray, QMimeData, QEvent
from PyQt5.QtGui import QDrag, QPainter, QPixmap, QMouseEvent, QResizeEvent, QPaintEvent
from PyQt5.QtWidgets import QSizePolicy, QWidget

from flexlay.color import Color
from flexlay.graphic_context import GraphicContext
from flexlay.math import Size, Rectf, Point, Origin
from flexlay.object_brush import ObjectBrush


class SuperTuxBadGuyData:

    def __init__(self) -> None:
        pass


class ObjectSelectorWidget(QWidget):

    def __init__(self, cell_w: int, cell_h: int, viewport: Size, parent: Optional[QWidget] = None) -> None:
        super().__init__(parent)

        self.viewport: Size = viewport
        self.cell_width: int = cell_w
        self.cell_height: int = cell_h
        self.brushes: list[ObjectBrush] = []
        self.has_focus = False

        self.index = 0

        self.mouse_pos: Optional[Point] = None
        self.click_pos: Optional[Point] = None

        self.mouse_over_tile = -1
        self.scale = 1.0
        self.drag_obj = -1

        self.setSizePolicy(QSizePolicy.Minimum, QSizePolicy.Expanding)
        self.setMouseTracking(True)

    def minimumSizeHint(self) -> QSize:
        columns = self.get_columns()
        min_rows = (len(self.brushes) + columns - 1) // columns
        return QSize(self.cell_width * self.get_columns(),
                     self.cell_height * min_rows)

    def resizeEvent(self, event: QResizeEvent) -> None:
        pass

    def get_columns(self) -> int:
        return int(self.viewport.width() // self.cell_width)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.LeftButton:
            if self.mouse_over_tile != -1:
                self.drag_obj = self.mouse_over_tile

                if self.drag_obj != -1:
                    drag = QDrag(self)
                    mime_data = QMimeData()
                    # GRUMBEL obj = SuperTuxBadGuyData()
                    data = QByteArray(pickle.dumps(self.drag_obj))
                    mime_data.setData("application/x-supertux-badguy", data)
                    drag.setMimeData(mime_data)

                    pixmap = QPixmap.fromImage(self.brushes[self.drag_obj].get_sprite().get_pixelbuffer().get_qimage())
                    drag.setPixmap(pixmap)
                    drag.setHotSpot(QPoint(self.brushes[self.drag_obj].get_sprite().width // 2,
                                           self.brushes[self.drag_obj].get_sprite().height // 2))
                    drag.exec()
                    self.drag_obj = -1

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        if event.button() == Qt.LeftButton:
            if self.drag_obj != -1:
                # releaseMouse()
                self.drag_obj = -1

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        self.mouse_pos = Point.from_qt(event.pos())

        cell_w = self.width() / self.get_columns()
        x = int(event.x() // cell_w)
        y = int(event.y() // self.cell_height)

        self.mouse_over_tile = y * self.get_columns() + x

        if self.mouse_over_tile < 0 or self.mouse_over_tile >= len(self.brushes):
            self.mouse_over_tile = -1

        if self.mouse_over_tile > -1:
            self.setToolTip(self.brushes[self.mouse_over_tile].get_data())

        self.repaint()

    def paintEvent(self, event: QPaintEvent) -> None:
        painter = QPainter(self)
        gc = GraphicContext(painter)

        for i in range(len(self.brushes)):
            x = i % self.get_columns()
            y = i // self.get_columns()

            cell_w = self.width() / self.get_columns()
            rect = Rectf(x * cell_w, y * self.cell_height,
                         (x + 1) * cell_w, (y + 1) * self.cell_height)

            if (x + y - 1) % 2 == 0:
                gc.fill_rect(rect, Color(224, 224, 224))
            else:
                gc.fill_rect(rect, Color(192, 192, 192))

            sprite = self.brushes[i].get_sprite()
            sprite.set_alignment(Origin.center, 0, 0)
            sprite.set_scale(min(1.0, self.cell_width / sprite.width),
                             min(1.0, self.cell_height / sprite.height))
            sprite.draw(rect.left + rect.width / 2,
                        rect.top + rect.height / 2,
                        gc)

            # highlight the current selection
            if self.mouse_over_tile == i and self.has_focus:
                gc.fill_rect(rect, Color(0, 0, 255, 20))

    def enterEvent(self, event: QEvent):
        self.has_focus = True

    def leaveEvent(self, event: QEvent):
        self.has_focus = False
        self.repaint()

    def add_brush(self, brush: ObjectBrush) -> None:
        self.brushes.append(brush)


# EOF #
