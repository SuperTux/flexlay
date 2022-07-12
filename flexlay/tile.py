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


from typing import Optional

import logging

from flexlay import Color, PixelBuffer
from flexlay.sprite import Sprite
from flexlay.sprite_brush import SpriteBrush


class Tile:

    def __init__(self, pixelbuffer: Optional[PixelBuffer] = None, sprite: Optional[Sprite] = None) -> None:
        self.provider: Optional[SpriteBrush] = None
        self.sprite: Optional[Sprite] = sprite
        self.pixelbuffer: Optional[PixelBuffer] = pixelbuffer
        self.transparent: bool = False
        self.color: Optional[Color] = None
        self.attribute_color: Color = Color(255, 255, 255)
        self.filename: Optional[str] = None

    def get_color(self) -> Color:
        if self.color is not None:
            return self.color
        else:
            self.color = self.calc_color()
            return self.color

    def get_attribute_color(self) -> Color:
        return self.attribute_color

    def get_sprite(self) -> Sprite:
        if self.sprite is not None:
            return self.sprite
        else:
            if self.provider is not None:
                self.sprite = self.provider.get_sprite()
            else:
                self.sprite = Sprite(self.get_pixelbuffer())

            return self.sprite

    def get_pixelbuffer(self) -> PixelBuffer:
        if self.pixelbuffer is not None:
            return self.pixelbuffer
        else:
            if self.provider is not None:
                self.pixelbuffer = self.provider.get_pixelbuffer()
                return self.pixelbuffer
            else:
                assert self.filename is not None
                self.pixelbuffer = PixelBuffer.from_file(self.filename)
                return self.pixelbuffer

    def calc_color(self) -> Color:
        logging.info("Tile::calc_color not implemented")
        return Color(255, 255, 255, 255)

    def get_filename(self) -> Optional[str]:
        return self.filename


# EOF #
