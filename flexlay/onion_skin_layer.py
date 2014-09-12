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


from flexlay import PixelBuffer, Surface, Canvas, Color, BlendFunc
from flexlay.gui import EditorMapComponent


class OnionSkinLayer:

    def __init__(self, width, height):
        self.SCALE = 1

        self.surface = Surface(PixelBuffer(width / self.SCALE, height / self.SCALE))
        self.surface2 = Surface(PixelBuffer(width / self.SCALE, height / self.SCALE))

        self.editormaps = []
        self.color = []

        self.canvas = Canvas(self.surface.to_cl())
        self.canvas.get_gc().clear(Color(0, 0, 0, 0).to_cl())
        self.canvas.get_gc().flush()
        self.canvas.sync_surface()

        self.canvas2 = Canvas(self.surface2.to_cl())
        self.canvas2.get_gc().clear(Color(0, 0, 0, 0).to_cl())
        self.canvas2.get_gc().flush()
        self.canvas2.sync_surface()

    def draw(self, gc):
        # FIXME: We need to stop onion layer to draw onto itself
        self.surface.set_blend_func(BlendFunc.one, BlendFunc.one_minus_src_alpha)
        self.surface.set_scale(self.SCALE, self.SCALE)
        self.surface.draw(0, 0)

    def has_bounding_rect(self):
        return False

    def clear(self):
        self.canvas.get_gc().clear(Color(0, 0, 0, 0).to_cl())
        self.canvas.sync_surface()

    def add_map(self, editor_map, color):
        self.editormaps.append(editor_map)
        self.color.append(color)

    def update(self):
        self.canvas.get_gc().clear(Color(0, 0, 0, 0).to_cl())
        for i, e in enumerate(self, self.editormaps):
            self.canvas2.get_gc().clear(Color(0, 0, 0, 0).to_cl())
            self.canvas2.get_gc().push_modelview()
            self.canvas2.get_gc().add_scale(1.0 / self.SCALE, 1.0 / self.SCALE)

            self.editormaps[i].draw(EditorMapComponent.current.get_gc_state(), self.canvas2.get_gc())

            self.canvas2.get_gc().pop_modelview()

            self.canvas2.sync_surface()

            self.surface2.set_color(self.color[i].to_cl())
            self.surface2.draw(0, 0, self.canvas.get_gc())
            self.canvas.sync_surface()


# EOF #
