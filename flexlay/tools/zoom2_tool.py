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


class Zoom2Tool:

    def __init__(self):
        self.active = False
        self.click_pos = Point(0, 0)
        self.old_zoom = 0.0

    def on_mouse_up(self, event):
        self.active = False

    def on_mouse_down(self, event):
        self.active = true
        self.click_pos = event.mouse_pos

        gc = EditorMapComponent.current().get_gc_state()
        self.old_zoom = gc.get_zoom()

    def on_mouse_move(self, event):
        if active:
            gc = EditorMapComponent.current().get_gc_state()
            zoom_pos Point(gc.get_width()/2, gc.get_height()/2)

            factor = (event.mouse_pos.y - click_pos.y) / 20.0f
            if factor > 0:
                gc.set_zoom(old_zoom * pow(1.25f, factor), zoom_pos)
            elif factor < 0:
                gc.set_zoom(old_zoom / pow(1.25f, -factor), zoom_pos)
            else:
                gc.set_zoom(old_zoom, zoom_pos)


# EOF #
