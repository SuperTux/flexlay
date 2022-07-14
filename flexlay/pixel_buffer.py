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


from PyQt5.sip import voidptr  # type: ignore
from PyQt5.QtCore import QSize
from PyQt5.QtGui import QImage

from flexlay.math import Size


class PixelBuffer:

    cache: dict[str, QImage] = {}

    @staticmethod
    def subregion_from_file(filename: str, x: int, y: int, w: int, h: int) -> 'PixelBuffer':
        from flexlay.blitter import blit_clear, blit_opaque

        source = PixelBuffer.from_file(filename)
        target = PixelBuffer.from_size(Size(w, h))
        blit_clear(target)
        blit_opaque(target, source, -x, -y)
        return target

    @staticmethod
    def from_file(filename: str) -> 'PixelBuffer':
        cached_image = PixelBuffer.cache.get(filename)
        if cached_image is None:
            cached_image = QImage(filename)
            if cached_image.isNull():
                raise RuntimeError(f"Failed to load image: {filename}")
            PixelBuffer.cache[filename] = cached_image

        return PixelBuffer(cached_image)

    @staticmethod
    def from_size(size: Size) -> 'PixelBuffer':
        return PixelBuffer(QImage(QSize(size.to_qt()), QImage.Format_ARGB32))

    def __init__(self, image: QImage) -> None:
        self.image = image

    def get_qimage(self) -> QImage:
        return self.image

    def lock(self) -> None:
        pass

    def unlock(self) -> None:
        pass

    @property
    def width(self) -> int:
        return self.image.width()

    @property
    def height(self) -> int:
        return self.image.height()

    def get_pitch(self) -> int:
        return self.image.bytesPerLine()

    def get_depth(self) -> int:
        return self.image.depth()

    def get_data(self) -> voidptr:
        return self.image.bits()


# EOF #
