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


from flexlay import Workspace, Color, Layer
from flexlay.gui import EditorMapComponent
from flexlay.math import Point, Pointf
from flexlay.tools import Tool


class LayerMoveTool(Tool):

    def __init__(self):
        self.scrolling = False
        self.click_pos = Point(0, 0)
        self.old_trans_offset = Pointf(0, 0)
        self.layer = None

    def find_closed_layer(self, pos):
        layer = None
        parent = Workspace.current().get_map()
        for i in range(0, parent.get_layer_count()):
            if parent.get_layer(i).get_bounding_rect().is_inside(Point(pos)):
                layer = parent.get_layer(i)
        return layer

    def draw(self, gc):
        for i in range(0, Workspace.current().get_map().get_layer_count()):
            layer = Workspace.current().get_map().get_layer(i)
            if layer.has_bounding_rect():
                rect = layer.get_bounding_rect()
                gc.draw_line(rect.left, rect.top, rect.right, rect.bottom,
                             Color(0, 255, 255))
                gc.draw_line(rect.left, rect.bottom, rect.right, rect.top,
                             Color(0, 255, 255))

    def on_mouse_up(self, event):
        if self.layer:
            self.scrolling = False
            self.update(event)
            EditorMapComponent.current().release_mouse()
            self.layer = Layer()

    def on_mouse_down(self, event):
        parent = EditorMapComponent.current()
        pos = parent.screen2world(event.mouse_pos)

        self.layer = self.find_closed_layer(pos)
        if self.layer:
            self.scrolling = True
            self.old_trans_offset = self.layer.get_pos()
            self.click_pos = pos
            EditorMapComponent.current().capture_mouse()

    def on_mouse_move(self, event):
        if self.layer and self.scrolling:
            self.update(event)

    def update(self, event):
        if self.layer:
            parent = EditorMapComponent.current()
            pos = parent.screen2world(event.mouse_pos)
            self.layer.set_pos(self.old_trans_offset + (pos - self.click_pos))


# EOF #
