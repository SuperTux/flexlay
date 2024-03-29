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


from typing import Any

import logging

from flexlay.color import Color
from flexlay.math import Pointf, Sizef, Rect, Rectf
from flexlay.objmap_control_point import ObjMapControlPoint
from flexlay.objmap_object import ObjMapObject
from flexlay.sprite import Sprite
from flexlay.graphic_context import GraphicContext


class ObjMapRectObject(ObjMapObject):

    def __init__(self, rect: Rectf, color: Color, metadata: Any) -> None:
        super().__init__(Pointf(rect.left, rect.top), metadata)

        self.size = Sizef(rect.width, rect.height)
        self.color = color

        self.cp_top_left = ObjMapControlPoint(Sprite.from_file("data/images/icons16/resize1.png"),
                                              Pointf(0, 0))

        self.cp_bottom_right = ObjMapControlPoint(Sprite.from_file("data/images/icons16/resize1.png"),
                                                  Pointf(0, 0))

        self.cp_top_right = ObjMapControlPoint(Sprite.from_file("data/images/icons16/resize2.png"),
                                               Pointf(0, 0))

        self.cp_bottom_left = ObjMapControlPoint(Sprite.from_file("data/images/icons16/resize2.png"),
                                                 Pointf(0, 0))

        self.cp_middle_left = ObjMapControlPoint(Sprite.from_file("data/images/icons16/resize_horz.png"),
                                                 Pointf(0, 0))
        self.cp_middle_right = ObjMapControlPoint(Sprite.from_file("data/images/icons16/resize_horz.png"),
                                                  Pointf(0, 0))
        self.cp_top_middle = ObjMapControlPoint(Sprite.from_file("data/images/icons16/resize_vert.png"),
                                                Pointf(0, 0))

        self.cp_bottom_middle = ObjMapControlPoint(Sprite.from_file("data/images/icons16/resize_vert.png"),
                                                   Pointf(0, 0))

        self.cp_top_right.sig_set_pos.connect(self.cp_top_right_move)
        self.cp_bottom_right.sig_set_pos.connect(self.cp_bottom_right_move)

        self.cp_top_left.sig_set_pos.connect(self.cp_top_left_move)
        self.cp_bottom_left.sig_set_pos.connect(self.cp_bottom_left_move)

        self.cp_middle_left.sig_set_pos.connect(self.cp_middle_left_move)
        self.cp_middle_right.sig_set_pos.connect(self.cp_middle_right_move)

        self.cp_top_middle.sig_set_pos.connect(self.cp_top_middle_move)
        self.cp_bottom_middle.sig_set_pos.connect(self.cp_bottom_middle_move)

    def set_rect(self, rect: Rect) -> None:
        self.pos = Pointf(rect.left, rect.top)
        self.size = Sizef(rect.width, rect.height)

    def cp_top_left_move(self, pos_: Pointf) -> None:
        self.size.width += self.pos.x - pos_.x
        self.size.height += self.pos.y - pos_.y
        self.pos = pos_

        self.normalize_rect()
        self.update_control_points()

    def cp_top_right_move(self, pos_: Pointf) -> None:
        self.size.width += pos_.x - (self.pos.x + self.size.width)
        self.size.height += self.pos.y - pos_.y

        self.pos.y = pos_.y

        self.normalize_rect()
        self.update_control_points()

    def cp_bottom_left_move(self, pos_: Pointf) -> None:
        self.size.width += self.pos.x - pos_.x
        self.size.height += pos_.y - (self.pos.y + self.size.height)
        self.pos.x = pos_.x

        self.normalize_rect()
        self.update_control_points()

    def cp_bottom_right_move(self, pos_: Pointf) -> None:
        self.size.width += pos_.x - (self.pos.x + self.size.width)
        self.size.height += pos_.y - (self.pos.y + self.size.height)

        self.normalize_rect()
        self.update_control_points()

    def cp_top_middle_move(self, pos_: Pointf) -> None:
        self.size.height += self.pos.y - pos_.y
        self.pos.y = pos_.y

        self.normalize_rect()
        self.update_control_points()

    def cp_bottom_middle_move(self, pos_: Pointf) -> None:
        self.size.height += pos_.y - (self.pos.y + self.size.height)

        self.normalize_rect()
        self.update_control_points()

    def cp_middle_left_move(self, pos_: Pointf) -> None:
        self.size.width += self.pos.x - pos_.x
        self.pos.x = pos_.x

        self.normalize_rect()
        self.update_control_points()

    def cp_middle_right_move(self, pos_: Pointf) -> None:
        self.size.width += pos_.x - (self.pos.x + self.size.width)

        self.normalize_rect()
        self.update_control_points()

    def normalize_rect(self) -> None:
        if self.size.width < 0:
            self.pos.x += self.size.width
            self.size.width = -self.size.width

        if self.size.height < 0:
            self.pos.y += self.size.height
            self.size.height = -self.size.height

    def get_rect(self) -> Rectf:
        return self.get_bound_rect()

    def set_color(self, color: Color) -> None:
        self.color = color

    def update_control_points(self) -> None:
        # Correct the control point position by 8 pixels (so that they align perfectly)
        pos = self.pos - Pointf(8, 8)

        self.cp_top_left.set_pos_raw(pos)
        self.cp_top_right.set_pos_raw(pos + Pointf(self.size.width, 0))
        self.cp_bottom_left.set_pos_raw(pos + Pointf(0, self.size.height))
        self.cp_bottom_right.set_pos_raw(pos + Pointf(self.size.width, self.size.height))
        self.cp_top_middle.set_pos_raw(pos + Pointf(self.size.width / 2, 0))
        self.cp_bottom_middle.set_pos_raw(pos + Pointf(self.size.width / 2, self.size.height))
        self.cp_middle_left.set_pos_raw(pos + Pointf(0, self.size.height / 2))
        self.cp_middle_right.set_pos_raw(pos + Pointf(self.size.width, self.size.height / 2))

    def draw(self, gc: GraphicContext) -> None:
        gc.fill_rect(self.get_bound_rect(), self.color)

    def get_bound_rect(self) -> Rectf:
        return Rectf.from_ps(self.pos, self.size)

    def add_control_points(self) -> None:
        self.update_control_points()
        logging.info("Adding control points...")

        from flexlay.tool_context import ToolContext
        assert ToolContext.current is not None
        objmap = ToolContext.current.object_layer
        assert objmap is not None
        objmap.add_control_point(self.cp_top_left)
        objmap.add_control_point(self.cp_top_right)
        objmap.add_control_point(self.cp_bottom_left)
        objmap.add_control_point(self.cp_bottom_right)
        objmap.add_control_point(self.cp_top_middle)
        objmap.add_control_point(self.cp_bottom_middle)
        objmap.add_control_point(self.cp_middle_left)
        objmap.add_control_point(self.cp_middle_right)


# EOF #
