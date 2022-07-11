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

from PyQt5.QtCore import QPoint

from flexlay.color import Color
from flexlay.math import Origin, Point, Size, Rect
from flexlay.pixel_buffer import PixelBuffer
from flexlay.graphic_context import GraphicContext


class Sprite:

    @staticmethod
    def from_pixelbuffer(pixelbuffer: PixelBuffer) -> 'Sprite':
        return Sprite(pixelbuffer)

    @staticmethod
    def from_file(filename: str) -> 'Sprite':
        return Sprite(PixelBuffer.from_file(filename), filename)

    def __init__(self, pixelbuffer: PixelBuffer, filename: str = "<unknown-source>") -> None:
        self.pixelbuffer = pixelbuffer
        self.filename = filename

        self.origin = Origin.top_left
        self.pos = Point(0, 0)
        self.scale = (1.0, 1.0)

    def draw(self, x: float, y: float, gc: GraphicContext) -> None:
        painter = gc.get_qt_painter()
        img = self.pixelbuffer.get_qimage()
        if not img:
            logging.error("Error: Sprite: Empty PixelBuffer: " + self.filename)
        else:
            scaled_width = self.width * self.scale[0]
            scaled_height = self.height * self.scale[1]
            origin = Origin.calc_origin(self.origin, Size(int(scaled_width), int(scaled_height)))
            if self.width > self.height:
                img = img.scaledToWidth(int(scaled_width))
            else:
                img = img.scaledToHeight(int(scaled_height))
            painter.drawImage(QPoint(int(x - origin.x),
                                     int(y - origin.y)),
                              img)

    @property
    def width(self) -> int:
        return self.pixelbuffer.width

    @property
    def height(self) -> int:
        return self.pixelbuffer.height

    def set_scale(self, x: float, y: float) -> None:
        self.scale = (x, y)

    def set_blend_func(self, src: int, dest: int) -> None:
        # m_sprite.set_blend_func(src, dest)
        pass

    def set_blend_func_separate(self, src: int, dest: int, src_alpha: int, dest_alpha: int) -> None:
        # m_sprite.set_blend_func_separate(src, dest, src_alpha, dest_alpha)
        pass

    def set_color(self, color: Color) -> None:
        # m_sprite.set_color(color)
        pass

    def set_alpha(self, alpha: float) -> None:
        # m_sprite.set_alpha(alpha)
        pass

    def set_alignment(self, origin: int, x: int, y: int) -> None:
        self.origin = origin
        self.pos = Point(x, y)

    def set_angle(self, angle: float) -> None:
        # m_sprite.set_angle(angle)
        pass

    def get_alignment(self) -> tuple[int, int, int]:
        return self.origin, self.pos.x, self.pos.y

    def get_scale(self) -> tuple[float, float]:
        # m_sprite.get_scale(x, y)
        return self.scale

    def add_frame(self, surface: Any, rect: Rect) -> None:
        # m_sprite.add_frame(surface, rect)
        pass

    def get_pixelbuffer(self) -> PixelBuffer:
        return self.pixelbuffer


# EOF #
