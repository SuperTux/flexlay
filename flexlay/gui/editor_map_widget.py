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


from typing import Optional, TYPE_CHECKING

from PyQt5.QtCore import QSize
from PyQt5.QtGui import (QColor, QPainter, QDragEnterEvent,
                         QDragLeaveEvent, QDropEvent,
                         QResizeEvent, QPaintEvent, QMouseEvent,
                         QWheelEvent)
from PyQt5.QtWidgets import QSizePolicy, QWidget  # QOpenGLWidget

from flexlay.input_event import InputEvent
from flexlay.graphic_context import GraphicContext
from flexlay.math import Pointf
from flexlay.util.signal import Signal

if TYPE_CHECKING:
    from flexlay.gui.editor_map_component import EditorMapComponent


class EditorMapWidget(QWidget):  # QOpenGLWidget

    def __init__(self, comp: 'EditorMapComponent', parent: Optional[QWidget]) -> None:
        super().__init__(parent)
        self.setMouseTracking(True)

        self.comp = comp
        self.workspace = comp.get_workspace()
        self.gc_state = comp.get_gc_state()
        self.painter = QPainter()
        self.gc = GraphicContext(self.painter, self.gc_state)

        pal = self.palette()
        pal.setColor(self.backgroundRole(), QColor(100, 0, 100))
        self.setPalette(pal)

        self.setAutoFillBackground(True)
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self.setAcceptDrops(True)
        self.sig_drop = Signal()
        self.sig_mouse_move = Signal()

    def dragEnterEvent(self, event: QDragEnterEvent) -> None:
        if event.mimeData().hasFormat("application/x-supertux-badguy"):
            event.accept()

    def dragLeaveEvent(self, event: QDragLeaveEvent) -> None:
        pass

    def dropEvent(self, event: QDropEvent) -> None:
        data = event.mimeData().data("application/x-supertux-badguy")
        pos = self.comp.screen2world(Pointf.from_qt(event.pos()))
        self.sig_drop(data, pos)

    def sizeHint(self) -> QSize:
        return QSize(1280, 800)

    def on_map_change(self) -> None:
        workspace = self.comp.get_workspace()
        if workspace and workspace.get_map():
            if workspace.get_map().has_bounding_rect():
                rect = workspace.get_map().get_bounding_rect()
                self.setMinimumSize(QSize(int(rect.width), int(rect.height)))

                pal = self.palette()
                pal.setColor(self.backgroundRole(), workspace.get_map().get_background_color().to_qt())
                self.setPalette(pal)

                self.repaint()

    def wheelEvent(self, event: QWheelEvent) -> None:
        num_degrees = event.angleDelta() / 8
        num_steps = int(num_degrees.manhattanLength() / 15)

        for _ in range(num_steps):
            if num_degrees.x() > 0 or num_degrees.y() > 0:
                self.comp.zoom_in(Pointf.from_qt(event.pos()))
            else:
                self.comp.zoom_out(Pointf.from_qt(event.pos()))

    def mouseMoveEvent(self, event: QMouseEvent) -> None:
        workspace = self.comp.get_workspace()
        ev = InputEvent.from_qt(event)
        workspace.mouse_move(ev)
        self.repaint()

        pos = self.comp.screen2world(Pointf.from_qt(event.pos()))
        self.sig_mouse_move(pos)

    def mousePressEvent(self, event: QMouseEvent) -> None:
        workspace = self.comp.get_workspace()
        ev = InputEvent.from_qt(event)
        workspace.mouse_down(ev)
        self.repaint()

    def mouseReleaseEvent(self, event: QMouseEvent) -> None:
        workspace = self.comp.get_workspace()
        ev = InputEvent.from_qt(event)
        workspace.mouse_up(ev)
        self.repaint()

    def paintEvent(self, event: QPaintEvent) -> None:
        self.painter.begin(self)

        # self.painter.setRenderHint(QPainter::Antialiasing)
        self.gc_state.push(self.gc)
        self.workspace.draw(self.gc)
        self.gc_state.pop(self.gc)

        self.painter.end()

    def resizeEvent(self, event: QResizeEvent) -> None:
        self.comp.get_gc_state().set_size(event.size().width(), event.size().height())


# EOF #
