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

from flexlay.util import Signal
from flexlay.math import Pointf, Rectf
from flexlay.graphic_context import GraphicContext


class ObjMapObject:

    def __init__(self, pos: Pointf, metadata: Any) -> None:
        self.to_draw: bool = True
        self.pos: Pointf = pos
        self.metadata: Any = metadata
        self.sig_select = Signal()
        self.sig_deselect = Signal()
        self.sig_move = Signal()

    def draw(self, gc: GraphicContext) -> None:
        pass

    def is_inside(self, click_pos: Pointf) -> bool:
        rect = self.get_bound_rect()
        if rect is None:
            return False
        return rect.is_inside(click_pos)

    def get_bound_rect(self) -> Optional[Rectf]:
        return None

    def get_pos(self) -> Pointf:
        return self.pos

    def set_pos(self, pos: Pointf) -> None:
        self.pos = pos

    def add_control_points(self) -> None:
        pass

    def update_control_points(self) -> None:
        pass


# EOF #
