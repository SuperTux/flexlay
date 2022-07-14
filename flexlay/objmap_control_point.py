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

from flexlay.graphic_context import GraphicContext
from flexlay.math import Pointf, Rectf, Origin, Sizef
from flexlay.sprite import Sprite
from flexlay.util.signal import Signal


class ObjMapControlPoint:

    def __init__(self, sprite: Sprite, pos: Pointf, metadata: Optional[Any] = None) -> None:
        self.sprite = sprite
        self.pos = pos
        self.metadata = metadata
        self.sig_set_pos = Signal()

    def draw(self, gc: GraphicContext) -> None:
        self.sprite.draw(int(self.pos.x), int(self.pos.y), gc)

    def set_pos_raw(self, p: Pointf) -> None:
        self.pos = p

    def set_pos(self, p: Pointf) -> None:
        self.sig_set_pos(p)

    def get_pos(self) -> Pointf:
        return self.pos

    def get_bound_rect(self) -> Rectf:
        origin_enum, align_x, align_y = self.sprite.get_alignment()
        align_x = -align_x
        origin = Origin.calc_originf(origin_enum,
                                     Sizef(self.sprite.width,
                                           self.sprite.height))

        return Rectf.from_ps(self.pos.copy() - origin - Pointf(align_x, align_y),
                             Sizef(self.sprite.width, self.sprite.height))


# EOF #
