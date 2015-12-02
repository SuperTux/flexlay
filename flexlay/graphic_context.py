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


from PyQt4.QtCore import QLineF
from flexlay.math import Rectf


class GraphicContext:

    def __init__(self, painter, state=None):
        self.painter = painter
        self.state = state

    def clear(self, color):
        pass  # GRUMBEL gc.clear(color)

    def draw_rect(self, rect, color):
        self.painter.setPen(color.to_qt())
        self.painter.drawRect(rect.to_qt())

    def fill_rect(self, rect, color):
        self.painter.fillRect(rect.to_qt(), color.to_qt())

    def draw_line(self, x1, y1, x2, y2, color):
        self.painter.setPen(color.to_qt())
        self.painter.drawLine(QLineF(x1, y1, x2, y2))

    def push_modelview(self):
        self.painter.save()

    def pop_modelview(self):
        self.painter.restore()

    def translate(self, x, y):
        self.painter.setViewTransformEnabled(True)
        self.painter.translate(x, y)

    def scale(self, x, y):
        self.painter.setViewTransformEnabled(True)
        self.painter.scale(x, y)

    def rotate(self, angle):
        self.painter.setViewTransformEnabled(True)
        self.painter.rotate(angle)

    def get_clip_rect(self):
        """
        If it's not obvious, this is rect containing all
        which is visible
        """
        if self.state:
            return self.state.get_clip_rect()
        else:
            return Rectf()

    def get_qt_painter(self):
        return self.painter

# EOF #
