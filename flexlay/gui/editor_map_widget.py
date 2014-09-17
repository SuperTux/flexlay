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

from PyQt4.QtCore import QSize
from PyQt4.QtGui import QColor, QPainter
from PyQt4.QtGui import QSizePolicy, QWidget

from flexlay.math import Point
from flexlay import InputEvent, GraphicContext
from flexlay.util import Signal


class EditorMapWidget(QWidget):

    def __init__(self, comp, parent):
        super().__init__(parent)
        self.setMouseTracking(True)

        self.comp = comp

        pal = self.palette()
        pal.setColor(self.backgroundRole(), QColor(100, 80, 100))
        self.setPalette(pal)

        self.setAutoFillBackground(True)
        self.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Expanding)
        self.setAcceptDrops(True)
        self.sig_drop = Signal()

    def dragEnterEvent(self, event):
        print("dragEnter:", event.mimeData().hasFormat("application/x-supertux-badguy"))
        if event.mimeData().hasFormat("application/x-supertux-badguy"):
            event.accept()

    def dragLeaveEvent(self, event):
        print("dragLeave")

    def dropEvent(self, event):
        print("drop happened:", event.pos().x(), event.pos().y())
        data = event.mimeData().data("application/x-supertux-badguy")
        pos = self.comp.screen2world(Point.from_qt(event.pos()))
        print("Received:", data)
        self.sig_drop(data, pos)

    def sizeHint(self):
        return QSize(1280, 800)

    def on_map_change(self):
        print("def on_map_change()")
        workspace = self.comp.get_workspace()
        if workspace and workspace.get_map():
            if workspace.get_map().has_bounding_rect():
                rect = workspace.get_map().get_bounding_rect()
                print("Setting minimum Size:", rect.width, rect.height)
                self.setMinimumSize(QSize(rect.width, rect.height))

                pal = self.palette()
                pal.setColor(self.backgroundRole(), workspace.get_map().get_background_color().to_qt())
                self.setPalette(pal)

                self.repaint()

    def wheelEvent(self, event):
        num_degrees = event.angleDelta().y() / 8
        num_steps = int(abs(num_degrees) / 15)

        for _ in range(num_steps):
            if num_degrees > 0:
                self.comp.zoom_in(Point.from_qt(event.pos()))
            else:
                self.comp.zoom_out(Point.from_qt(event.pos()))

    def mouseMoveEvent(self, event):
        workspace = self.comp.get_workspace()
        ev = InputEvent.from_qt(event)
        workspace.mouse_move(ev)
        self.repaint()

    def mousePressEvent(self, event):
        workspace = self.comp.get_workspace()
        ev = InputEvent.from_qt(event)
        workspace.mouse_down(ev)
        self.repaint()

    def mouseReleaseEvent(self, event):
        workspace = self.comp.get_workspace()
        ev = InputEvent.from_qt(event)
        workspace.mouse_up(ev)
        self.repaint()

    def paintEvent(self, event):
        painter = QPainter()
        painter.begin(self)

        # painter.setRenderHint(QPainter::Antialiasing)

        workspace = self.comp.get_workspace()
        gc = GraphicContext(painter, self.comp.get_gc_state())
        self.comp.get_gc_state().push(gc)
        workspace.draw(gc)
        self.comp.get_gc_state().pop(gc)

        painter.end()

    def resizeEvent(self, event):
        self.comp.get_gc_state().set_size(event.size().width(), event.size().height())


# EOF #
