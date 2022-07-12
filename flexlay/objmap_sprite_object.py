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

from flexlay import ObjMapObject
from flexlay.math import Origin, Sizef, Rectf
from flexlay.graphic_context import GraphicContext
from flexlay.sprite import Sprite
from flexlay.math import Pointf


class ObjMapSpriteObject(ObjMapObject):

    def __init__(self, sprite: Sprite, pos: Pointf, metadata: Any) -> None:
        super().__init__(pos, metadata)

        self.sprite: Sprite = sprite
        self.pos: Pointf = pos
        self.metadata: Any = metadata

    def draw(self, gc: GraphicContext) -> None:
        self.sprite.draw(self.pos.x, self.pos.y, gc)

    def get_bound_rect(self) -> Rectf:
        align = Pointf(0, 0)
        origin_e = Origin()

        origin_e, align.x, align.y = self.sprite.get_alignment()
        origin = Origin.calc_originf(origin_e, Sizef(self.sprite.width,
                                                     self.sprite.height))
        align.x = -align.x

        # FIXME: This looks a bit hacky
        scale_x, scale_y = self.sprite.get_scale()

        if scale_x < 0:
            align.x += self.sprite.width

        if scale_y < 0:
            align.y += self.sprite.height

        # if (scale_x > 1.0f && scale_y > 1.0f)
        #    return Rectf(pos - origin - align,
        #                   Sizef(sprite.width * scale_x, sprite.height * scale_y))
        #  else
        return Rectf.from_ps(self.pos - origin - align,
                             Sizef(self.sprite.width, self.sprite.height))

    def flip_vertical(self) -> None:
        scale_x, scale_y = self.sprite.get_scale()
        self.sprite.set_scale(scale_x, -scale_y)
        if scale_y < 0:
            self.pos.y -= self.sprite.height
        else:
            self.pos.y += self.sprite.height

    def flip_horizontal(self) -> None:
        scale_x, scale_y = self.sprite.get_scale()
        self.sprite.set_scale(-scale_x, scale_y)
        if scale_x < 0:
            self.pos.x -= self.sprite.width
        else:
            self.pos.x += self.sprite.width

    def set_sprite(self, sprite: Sprite) -> None:
        self.sprite = sprite

    def set_rotate(self, angle: float) -> None:
        self.sprite.set_angle(angle)


# EOF #
