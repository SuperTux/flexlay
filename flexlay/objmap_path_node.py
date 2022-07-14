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


from typing import Any, Optional

from flexlay.objmap_object import ObjMapObject
from flexlay.color import Color
from flexlay.math import Pointf, Sizef, Rectf


class ObjMapPathNode(ObjMapObject):

    def __init__(self, pos: Pointf, data) -> None:
        super().__init__(pos, data)

        self.pos: Pointf = pos
        self.data = data
        self.prev_node: Optional['ObjMapPathNode'] = None
        self.next_node: Optional['ObjMapPathNode'] = None

    def draw(self, gc):
        gc.fill_rect(Rectf.from_ps(self.pos - Pointf(16, 16),
                                   Sizef(32, 32)),
                     Color(200, 255, 200))
        if self.next_node:
            gc.draw_line(self.pos.x, self.pos.y,
                         (self.pos.x + self.next_node.pos.x) / 2,
                         (self.pos.y + self.next_node.pos.y) / 2,
                         Color(255, 255, 0))

            gc.draw_line((self.pos.x + self.next_node.pos.x) / 2,
                         (self.pos.y + self.next_node.pos.y) / 2,
                         self.next_node.pos.x,
                         self.next_node.pos.y,
                         Color(255, 0, 0))

    def get_bound_rect(self) -> Rectf:
        return Rectf.from_ps(self.pos - Pointf(16, 16), Sizef(32, 32))

    def disconnect(self) -> None:
        assert self.next_node is not None
        assert self.prev_node is not None

        if self.next_node.prev_node is not None:
            self.next_node.prev_node = None

        if self.prev_node.prev_node is not None:
            self.prev_node.next_node = None

        self.next_node = None
        self.prev_node = None

    def connect(self, node: 'ObjMapPathNode') -> None:
        assert node != self

        # ensure that each node links exactly to one prev and one next node
        if node.prev_node:
            node.prev_node.next_node = None
            node.prev_node = None

        self.next_node = node
        node.prev_node = self


# EOF #
