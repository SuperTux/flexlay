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


from flexlay.wip.stroke import Stroke
from flexlay.wip.sprite_stroke_drawer import SpriteStrokeDrawer
from flexlay.wip.bitmap_layer import BitmapLayer
from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.math import Point


class Mouse:

    pass


class DrawerProperties:

    pass


class InputEvent:

    pass


class Dab:

    pass


class SketchStrokeTool:

    def __init__(self):
        self.drawing = False
        self.stroke = Stroke()
        self.drawer = SpriteStrokeDrawer()

    def draw(self, gc):
        if self.drawing:
            # FIXME: This translation is a bit ugly, layer position should be handled somewhat different
            gc.push_modelview()
            gc.add_translate(BitmapLayer.current.get_pos().x,
                             BitmapLayer.current.get_pos().y)
            self.stroke.draw(0)
            gc.pop_modelview()
        else:
            parent = EditorMapComponent.current
            p = parent.screen2world(Point(Mouse.get_x(), Mouse.get_y()))
            s = DrawerProperties.current.get_brush().get_sprite()
            s.set_color(DrawerProperties.current.get_color())
            # FIXME: when using mouse 1.0, when tablet 0.5
            s.set_scale(DrawerProperties.current.get_size() * 0.5,
                        DrawerProperties.current.get_size() * 0.5)
            s.set_alpha(0.5)
            s.draw(p.x, p.y)

    def on_mouse_up(self, event):
        if event.kind == InputEvent.MOUSE_LEFT and self.drawing:
            self.drawing = False
            parent = EditorMapComponent.current
            parent.release_mouse()
            self.add_dab(event)
            BitmapLayer.current.add_stroke(self.stroke)

    def on_mouse_down(self, event):
        if event.kind == InputEvent.MOUSE_LEFT:
            self.drawing = True
            parent = EditorMapComponent.current
            parent.grab_mouse()
            self.stroke = Stroke()
            self.stroke.set_drawer(self.drawer.copy())
            self.add_dab(event)

    def add_dab(self, event):
        parent = EditorMapComponent.current
        p = parent.screen2world(event.mouse_pos)

        # FIXME: This is ugly, events relative to the layer should be handled somewhat differently
        dab = Dab(p.x - BitmapLayer.current.get_pos().x,
                  p.y - BitmapLayer.current.get_pos().y)

        # FIXME: Make tablet configurable
        # if (CL_Display.get_current_window().get_ic().get_mouse_count() >= 4)
        # {
        #   CL_InputDevice tablet = CL_Display.get_current_window().get_ic().get_mouse(5)
        #   print("Mouse Count: " << CL_Display.get_current_window().get_ic().get_mouse_count() << std.endl
        #   std.cout << tablet.get_name() << ": "
        #   for(int i = 0 i < tablet.get_axis_count() ++i)
        #     std.cout << tablet.get_axis(i) << " "
        #   std.cout << std.endl
        #   dab.pressure = tablet.get_axis(2)
        #   dab.tilt.x   = tablet.get_axis(3)
        #   dab.tilt.y   = tablet.get_axis(4)
        # }

        # std.cout << dab.pressure << " " << dab.tilt.x << " " << dab.tilt.y << std.endl

        if dab.pressure == 0:  # most likly we are using the mouse
            dab.pressure = 1.0

        self.stroke.add_dab(dab)

    def on_mouse_move(self, event):
        if self.drawing:
            self.add_dab(event)

    def get_drawer(self):
        return self.drawer


# EOF #
