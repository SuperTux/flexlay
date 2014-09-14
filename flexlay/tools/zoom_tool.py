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


from flexlay.math import Rect, Rectf
from flexlay import Color, InputEvent
from ..gui.editor_map_component import EditorMapComponent
from flexlay.tools import Tool


class ZoomTool(Tool):

    NONE_STATE = 0
    CREATE_ZOOM_RECT = 1

    def __init__(self):
        super().__init__()

        self.state = ZoomTool.NONE_STATE
        self.zoom_rect = Rect()

    def draw(self, gc):
        if self.state == ZoomTool.CREATE_ZOOM_RECT:
            tmp = Rectf(self.zoom_rect)
            tmp.normalize()
            gc.fill_rect(tmp, Color(255, 255, 0, 50))
            gc.draw_rect(tmp, Color(255, 255, 0, 200))

    def on_mouse_up(self, event):
        parent = EditorMapComponent.current

        if event.kind != InputEvent.MOUSE_RIGHT:
            if self.state == ZoomTool.CREATE_ZOOM_RECT:
                self.state = ZoomTool.NONE_STATE
                parent.release_mouse()

                pos = parent.screen2world(event.mouse_pos)
                self.zoom_rect.right = pos.x
                self.zoom_rect.bottom = pos.y
                self.zoom_rect.normalize()
                if self.zoom_rect.width > 10 and self.zoom_rect.height > 10:
                    parent.zoom_to(self.zoom_rect)

    def on_mouse_down(self, event):
        parent = EditorMapComponent.current

        if event.kind == InputEvent.MOUSE_RIGHT:
            if self.state == ZoomTool.NONE_STATE:
                parent.zoom_out(event.mouse_pos)
                parent.zoom_out(event.mouse_pos)

        else:
            if self.state == ZoomTool.NONE_STATE:
                self.state = ZoomTool.CREATE_ZOOM_RECT
                parent.capture_mouse()

                pos = parent.screen2world(event.mouse_pos)
                self.zoom_rect.left = pos.x
                self.zoom_rect.top = pos.y
                self.zoom_rect.right = pos.x
                self.zoom_rect.bottom = pos.y

    def on_mouse_move(self, event):
        parent = EditorMapComponent.current

        if self.state == ZoomTool.CREATE_ZOOM_RECT:
            pos = parent.screen2world(event.mouse_pos)
            self.zoom_rect.right = pos.x
            self.zoom_rect.bottom = pos.y


# EOF #
