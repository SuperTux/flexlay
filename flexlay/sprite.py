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


import logging

from PyQt5.QtCore import QPoint

from flexlay.math import Origin, Point, Size
from flexlay.pixel_buffer import PixelBuffer


class Sprite:
    @staticmethod
    def from_pixelbuffer(pixelbuffer):
        return Sprite(pixelbuffer)

    @staticmethod
    def from_file(filename):
        return Sprite(PixelBuffer.from_file(filename), filename)

    def __init__(self, pixelbuffer, filename="<unknown-source>"):
        self.pixelbuffer = pixelbuffer
        self.filename = filename

        self.origin = Origin.top_left
        self.pos = Point(0, 0)
        self.scale = (1.0, 1.0)

    def draw(self, x, y, gc):
        painter = gc.get_qt_painter()
        img = self.pixelbuffer.get_qimage()
        if not img:
            logging.error("Error: Sprite: Empty PixelBuffer: " + self.filename)
        else:
            scaled_width = self.width * self.scale[0]
            scaled_height = self.height * self.scale[1]
            origin = Origin.calc_origin(self.origin, Size(scaled_width, scaled_height))
            if self.width > self.height:
                img = img.scaledToWidth(scaled_width)
            else:
                img = img.scaledToHeight(scaled_height)
            painter.drawImage(QPoint(x - origin.x, y - origin.y), img)

    @property
    def width(self):
        return self.pixelbuffer.width

    @property
    def height(self):
        return self.pixelbuffer.height

    def set_scale(self, x, y):
        self.scale = (x, y)
        pass

    def set_blend_func(self, src, dest):
        # m_sprite.set_blend_func(src, dest)
        pass

    def set_blend_func_separate(self, src, dest, src_alpha, dest_alpha):
        # m_sprite.set_blend_func_separate(src, dest, src_alpha, dest_alpha)
        pass

    def set_color(self, color):
        # m_sprite.set_color(color)
        pass

    def set_alpha(self, alpha):
        # m_sprite.set_alpha(alpha)
        pass

    def set_alignment(self, origin, x, y):
        self.origin = origin
        self.pos = Point(x, y)

    def set_angle(self, angle):
        # m_sprite.set_angle(angle)
        pass

    def get_alignment(self):
        return self.origin, self.pos.x, self.pos.y

    def get_scale(self):
        # m_sprite.get_scale(x, y)
        return self.scale

    def add_frame(self, surface, rect):
        # m_sprite.add_frame(surface, rect)
        pass

    def get_pixelbuffer(self):
        return self.pixelbuffer


# EOF #
