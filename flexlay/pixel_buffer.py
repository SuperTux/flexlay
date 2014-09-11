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


from PyQt5.Core import QSize
from PyQt5.Gui import QImage


class PixelBuffer:

    @staticmethod
    def from_file(filename):
        pixelbuffer = PixelBuffer()
        pixelbuffer.image = QImage(filename.c_str())
        print("loading:", filename, " -> ", pixelbuffer.image)

        if not pixelbuffer.image:
            assert False, "Failed to load image, fatal"

        return pixelbuffer

    def __init__(self, width=0, height=0):
        if width != 0 and height != 0:
            self.image = QImage(QSize(width, height), QImage.Format_ARGB32)
        else:
            self.image = None

    def get_qimage(self):
        return self.image

    def lock(self):
        pass

    def unlock(self):
        pass

    def get_width(self):
        return self.image.width()

    def get_height(self):
        return self.image.height()

    def get_pitch(self):
        return self.image.bytesPerLine()

    def get_depth(self):
        return self.image.depth()

    def get_data(self):
        return self.image.bits()


# EOF #
