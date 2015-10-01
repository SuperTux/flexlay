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


from flexlay.math import Rectf, Pointf, Point, Sizef, Rect, Size
from flexlay import PixelBuffer, Canvas, Color, BlendFunc, Surface, Layer


class BitmapLayer(Layer):

    current = None

    def __init__(self, width, height):
        super().__init__()

        BitmapLayer.current = self

        self.strokes = []
        self.pixelbuffer = PixelBuffer(width, height)
        self.surface = None
        self.canvas = Canvas()
        self.last_pos = Point()
        self.surface = None

    def draw(self, gc):
        # Nothing to draw, so we go byebye
        if self.strokes != []:
            self.surface.set_blend_func(BlendFunc.one, BlendFunc.one_minus_src_alpha)
            self.surface.draw(self.pos.x, self.pos.y)

        gc.draw_rect(self.get_bounding_rect(), Color(155, 155, 155, 100))

    def get_bound_rect(self):
        return Rectf(Pointf(self.pos),
                     Sizef(self.surface.width, self.surface.height))

    def get_bounding_rect(self):
        # FIXME: Do we need to handle its position here or does the Layer keep care of that?
        return Rect(Point(0, 0),
                    Size(self.surface.width, self.surface.height))

    def has_bounding_rect(self):
        return True

    def add_stroke(self, stroke):
        if stroke.get_dab_count() > 0:
            self.strokes.append(stroke)
            stroke.draw(self.canvas.get_gc())
            # FIXME: doesn't sync when manually manipulating the canvas
            self.canvas.get_gc().flush()
            self.canvas.sync_surface()

    def get_strokes(self):
        return self.strokes

    def get_background_surface(self):
        return self.surface

    def get_canvas(self):
        return self.canvas

    def set_pixeldata(self, pbuffer):
        # self.canvas.set_pixeldata(pbuffer)
        Surface(self.pbuffer).draw(0, 0, self.canvas.get_gc())
        self.canvas.get_gc().flush()

    def get_pixeldata(self):
        return self.canvas.get_pixeldata()


# EOF #
