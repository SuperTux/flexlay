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


from flexlay import Color, Layer
from flexlay.math import Rect
from flexlay.util import Signal


class EditorMap:

    def __init__(self):
        self.background_color = Color(100, 80, 100)
        self.foreground_color = Color(255, 80, 255)
        self.modified = False
        self.serial = 0
        self._has_bounding_rect = False
        self.bounding_rect = Rect(0, 0, 0, 0)
        self.layers = []
        self.redo_stack = []
        self.undo_stack = []
        self.sig_change = Signal()
        self.metadata = None

    def add_layer(self, layer, pos=-1):
        assert pos == -1 or (pos >= 0 and pos < len(self.layers))

        if pos == -1:  # insert at last pos
            self.layers.append(layer)
        else:
            self.layers.insert(pos, layer)

        self.serial += 1

    def draw_gui(self, gc):
        rect = self.get_bounding_rect()
        if rect != Rect(0, 0, 0, 0):
            gc.draw_rect(rect, self.foreground_color)

    def draw(self, gc):
        for layer in self.layers:
            layer.draw(gc)

    def is_modified(self):
        return self.modified

    def set_unmodified(self):
        self.modified = False

    def modify(self):
        self.modified = True
        self.serial += 1

    def get_serial(self):
        return self.serial

    def get_layer_count(self):
        return len(self.layers)

    def get_layer(self, i):
        if i >= 0 and i < len(self.layers):
            return self.layers[i]
        else:
            return Layer()

    def has_bounding_rect(self):
        return self._has_bounding_rect

    def set_bounding_rect(self, rect):
        if rect != Rect(0, 0, 0, 0):
            self._has_bounding_rect = True
            self.bounding_rect = rect
        else:
            self._has_bounding_rect = False
            self.bounding_rect = rect

    def get_bounding_rect(self):
        if self._has_bounding_rect:
            return self.bounding_rect
        else:
            init = False
            rect = Rect(0, 0, 0, 0)

            for layer in self.layers:
                if layer.has_bounding_rect():
                    if not init:
                        rect = layer.get_bounding_rect()
                        init = True
                    else:
                        other = layer.get_bounding_rect()
                        rect.top = min(rect.top, other.top)
                        rect.bottom = max(rect.bottom, other.bottom)
                        rect.left = min(rect.left, other.left)
                        rect.right = max(rect.right, other.right)

            return rect

    def set_background_color(self, color):
        self.background_color = color

    def get_background_color(self):
        return self.background_color

    def execute(self, command):
        self.redo_stack.clear()
        command.execute()
        self.undo_stack.append(command)
        self.sig_change()

    def undo(self):
        if self.undo_stack:
            command = self.undo_stack[-1]
            self.undo_stack.pop()
            command.undo()
            self.redo_stack.append(command)
            self.sig_change()

    def redo(self):
        if self.redo_stack:
            command = self.redo_stack[-1]
            self.redo_stack.pop()
            command.redo()
            self.undo_stack.append(command)
            self.sig_change()

    def undo_stack_size(self):
        return len(self.undo_stack)

    def redo_stack_size(self):
        return len(self.redo_stack)


# EOF #
