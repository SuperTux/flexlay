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


from flexlay import Color, PixelBuffer, Sprite


class Tile:

    # FIXME: Need proper constructors
    def __init__(self, pixelbuffer=None, sprite=None):
        self.provider = None
        self.sprite = sprite
        self.pixelbuffer = pixelbuffer
        self.transparent = False
        self.color = None
        self.attribute_color = Color(255, 255, 255)
        self.filename = None

    def get_color(self):
        if self.color is not None:
            return self.color
        else:
            self.color = self.calc_color()
            return self.color

    def get_attribute_color(self):
        return self.attribute_color

    def get_sprite(self):
        if self.sprite is not None:
            return self.sprite
        else:
            if self.provider is not None:
                self.sprite = self.provider.get_sprite()
            else:
                self.sprite = Sprite(self.get_pixelbuffer())

            return self.sprite

    def get_pixelbuffer(self):
        if self.pixelbuffer is not None:
            return self.pixelbuffer
        else:
            if self.provider is not None:
                self.pixelbuffer = self.provider.get_pixelbuffer()
                return self.pixelbuffer
            else:
                self.pixelbuffer = PixelBuffer.from_file(self.filename)
                return self.pixelbuffer

    def calc_color(self):
        print("Tile::calc_color not implemented")
        return Color(255, 255, 255, 255)

    def get_filename(self):
        return self.filename


# EOF #
