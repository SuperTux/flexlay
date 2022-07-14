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


from typing import Optional, TYPE_CHECKING

from PyQt5.QtCore import QLineF
from PyQt5.QtGui import QPainter, QPainterPath

from flexlay.color import Color
from flexlay.math.rect import Rectf

if TYPE_CHECKING:
    from flexlay.graphic_context_state import GraphicContextState


class GraphicContext:

    def __init__(self, painter: QPainter, state: Optional['GraphicContextState'] = None) -> None:
        self.painter = painter
        self.state = state

    def clear(self, color: Color) -> None:
        pass  # GRUMBEL gc.clear(color)

    def draw_rect(self, rect: Rectf, color: Color, radius: int = 0) -> None:
        if radius == 0:
            self.painter.setPen(color.to_qt())
            self.painter.drawRect(rect.to_qt())
            return

        self.painter.setPen(color.to_qt())
        self.painter.drawRoundedRect(rect.to_qt(), radius, radius)

    def fill_rect(self, rect: Rectf, color: Color, radius: int = 0) -> None:
        if radius == 0:
            self.painter.fillRect(rect.to_qt(), color.to_qt())
            return

        path = QPainterPath()
        path.addRoundedRect(rect.to_qt(), radius, radius)
        self.painter.fillPath(path, color.to_qt())

    def draw_line(self, x1: float, y1: float, x2: float, y2: float, color: Color) -> None:
        self.painter.setPen(color.to_qt())
        self.painter.drawLine(QLineF(x1, y1, x2, y2))

    def push_modelview(self) -> None:
        self.painter.save()

    def pop_modelview(self) -> None:
        self.painter.restore()

    def translate(self, x: float, y: float) -> None:
        self.painter.setViewTransformEnabled(True)
        self.painter.translate(x, y)

    def scale(self, x: float, y: float) -> None:
        self.painter.setViewTransformEnabled(True)
        self.painter.scale(x, y)

    def rotate(self, angle: float) -> None:
        self.painter.setViewTransformEnabled(True)
        self.painter.rotate(angle)

    def get_clip_rect(self) -> Rectf:
        """
        If it's not obvious, this is rect containing all
        which is visible
        """
        if self.state:
            return self.state.get_clip_rect()
        else:
            return Rectf.zero()

    def get_qt_painter(self) -> QPainter:
        return self.painter


# EOF #
