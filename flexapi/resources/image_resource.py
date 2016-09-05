# Flexlay - A Generic 2D Game Editor
#
# ISC License
# Copyright (C) 2016 Karkus476 <karkus476@yahoo.com>
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR ON SEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

from PyQt4.QtGui import QPixmap, QImage

from .flexlay_resource import FlexlayResource
from ..flexlay_error import ResourceError


class ImageResource(FlexlayResource):
    def __init__(self, qimage):
        if not isinstance(qimage, QImage):
            raise ResourceError("qimage must be instance of QImage")
        self.qimage = qimage
        self.pixmap = None
        
    def get_subregion(self, x, y, w, h):
        """Returns an ImageResource from the specified subregion"""
        return ImageResource(self.qimage.copy(x, y, w, h))

    def get_pixmap(self):
        if self.pixmap is None:
            self.pixmap = QPixmap()
            self.pixmap.convertFromImage(self.qimage)
            if self.pixmap.isNull():
                self.pixmap = None
                raise ResourceError("Could not convert ImageResource to pixmap.")
            return self.pixmap
        else:
            return self.pixmap

    def get_width(self):
        return self.qimage.width()

    def get_height(self):
        return self.qimage.height()
