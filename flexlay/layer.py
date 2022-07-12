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


from typing import Any

from flexlay.graphic_context import GraphicContext
from flexlay.math import Pointf, Rectf


class Layer:

    def __init__(self) -> None:
        self.data = None
        self.pos = Pointf(0, 0)
        self.metadata: Any = None

    def draw(self, gc: GraphicContext) -> None:
        if self.pos.x != 0 or self.pos.y != 0:
            gc.push_modelview()
            gc.translate(self.pos.x, self.pos.y)
            self.draw(gc)
            gc.pop_modelview()
        else:
            self.draw(gc)

    def has_bounding_rect(self) -> bool:
        return self.has_bounding_rect()

    def get_bounding_rect(self) -> Rectf:
        rect = self.get_bounding_rect()
        rect.left += self.pos.x
        rect.top += self.pos.y
        rect.right += self.pos.x
        rect.bottom += self.pos.y
        return rect

    def set_pos(self, pos: Pointf) -> None:
        self.pos = pos

    def get_pos(self) -> Pointf:
        return self.pos


# EOF #
