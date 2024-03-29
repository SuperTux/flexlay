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

import math

from flexlay.graphic_context import GraphicContext
from flexlay.math import Pointf, Rectf, Sizef


class GraphicContextState:

    def __init__(self, w: int = 1, h: int = 1) -> None:
        self.width: int = w
        self.height: int = h
        self.offset: Pointf = Pointf(0, 0)
        self.zoom: float = 1.0
        self.rotation: float = 0

    def set_size(self, w: int, h: int) -> None:
        self.width = w
        self.height = h

    def push(self, gc: GraphicContext) -> None:
        gc.push_modelview()

        gc.translate(self.width / 2, self.height / 2)
        gc.rotate(self.rotation)
        gc.translate(-self.width / 2, -self.height / 2)

        gc.scale(self.get_zoom(), self.get_zoom())
        gc.translate(self.offset.x, self.offset.y)

    def pop(self, gc: GraphicContext) -> None:
        gc.pop_modelview()

    def get_clip_rect(self) -> Rectf:
        return Rectf.from_ps(Pointf(-self.offset.x,
                                    -self.offset.y),
                             Sizef(self.width / self.zoom,
                                   self.height / self.zoom))

    def set_pos(self, pos: Pointf) -> None:
        self.offset.x = -pos.x + (self.width / 2 / self.zoom)
        self.offset.y = -pos.y + (self.height / 2 / self.zoom)

    def get_pos(self) -> Pointf:
        return Pointf(-self.offset.x + (self.width / 2 / self.zoom),
                      -self.offset.y + (self.height / 2 / self.zoom))

    def set_zoom(self, z: float, pos: Optional[Pointf] = None) -> None:
        if pos is None:
            self.zoom = z
        else:
            old_zoom = self.zoom
            self.set_zoom(z)
            self.offset.x = pos.x / self.zoom - pos.x / old_zoom + self.offset.x
            self.offset.y = pos.y / self.zoom - pos.y / old_zoom + self.offset.y

    def get_zoom(self) -> float:
        return self.zoom

    def zoom_to(self, rect: Rectf) -> None:
        center_x = (rect.left + rect.right) / 2.0
        center_y = (rect.top + rect.bottom) / 2.0

        width = rect.right - rect.left
        height = rect.bottom - rect.top
        screen_relation = float(self.height) / float(self.width)
        rect_relation = height / width

        if rect_relation < screen_relation:  # take width, ignore height
            self.zoom = self.width / width
        else:  # take height, ignore width
            self.zoom = self.height / height

        self.offset.x = (self.width / (2 * self.zoom)) - center_x
        self.offset.y = (self.height / (2 * self.zoom)) - center_y

    def screen2world(self, pos: Pointf) -> Pointf:
        pos = pos.copy()
        sa = math.sin(-self.rotation / 180.0 * math.pi)
        ca = math.cos(-self.rotation / 180.0 * math.pi)

        dx = pos.x - self.width / 2
        dy = pos.y - self.height / 2

        pos.x = self.width / 2 + (ca * dx - sa * dy)
        pos.y = self.height / 2 + (sa * dx + ca * dy)

        return Pointf((float(pos.x) / self.zoom) - self.offset.x,
                      (float(pos.y) / self.zoom) - self.offset.y)

    def set_rotation(self, angle: float) -> None:
        self.rotation = angle

    def get_rotation(self) -> float:
        return self.rotation


# EOF #
