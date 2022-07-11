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


from PyQt5.QtGui import QColor

from flexlay.blend_func import BlendFunc
from flexlay.gui import EditorMapComponent
from flexlay.layer import Layer
from flexlay.math import Point
from flexlay.wip.canvas import Canvas
from flexlay.wip.display import Display
from flexlay.wip.pixel_buffer import PixelBuffer


class SketchLayer(Layer):

    def __init__(self) -> None:
        self.strokes = []

        self.surface = PixelBuffer(Display.width, Display.height)
        self.canvas = Canvas(self.surface)
        self.last_zoom = 0.0
        self.last_rot = 0.0
        self.last_pos = Point()

    def add_stroke(self, stroke):
        if stroke.get_dab_count() > 0:
            self.strokes.append(self.stroke)

            if self.canvas:
                parent = EditorMapComponent.current
                parent.get_gc_state().push(self.canvas.get_gc())
                stroke.draw(self.canvas.get_gc())
                parent.get_gc_state().pop(self.canvas.get_gc())
                self.canvas.sync_surface()

    def draw(self, gc):
        # Nothing to draw, so we go byebye
        if self.strokes.empty():
            return

        if self.canvas is None:
            # Direct Drawing, slow
            for stroke in self.strokes():
                stroke.draw(0)
        else:
            # Draw to canvas
            if self.last_zoom != gc.state.get_zoom() or \
               self.last_pos != gc.state.get_pos() or \
               self.last_rot != gc.state.get_rotation():

                # Rerender the image
                self.last_zoom = gc.state.get_zoom()
                self.last_pos = gc.state.get_pos()
                self.last_rot = gc.state.get_rotation()

                gc.state.push(self.canvas.get_gc())
                self.canvas.get_gc().clear(QColor(0, 0, 0, 0))
                # canvas.get_gc().clear(Color.white)

                visible_area = self.state.get_clip_rect()

                for stroke in self.strokes:
                    # canvas.get_gc().draw_rect(i.get_bounding_rect(), Color(0, 255, 0))
                    # canvas.get_gc().flush()
                    if visible_area.is_overlapped(stroke.get_bounding_rect()):
                        stroke.draw(self.canvas.get_gc())

            self.state.pop(self.canvas.get_gc())
            self.canvas.sync_surface()

            self.surface.set_blend_func(BlendFunc.one, BlendFunc.one_minus_src_alpha)

            matrix = Display.get_modelview()
            gc.pop_modelview()
            self.surface.draw(0, 0)
            gc.set_modelview(matrix)
            # FIXME: I think we need the line below, however with it it
            # doesn't work, without it, it does, ClanLib bug or just
            # consfusing function names?
            # gc.push_modelview()

    def has_bounding_rect(self):
        return False

    def get_strokes(self):
        return self.strokes

    def get_background_surface(self):
        return self.surface


# EOF #
