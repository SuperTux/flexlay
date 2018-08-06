# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from flexlay import Workspace
from flexlay.math import Point
from flexlay.util import get_value_from_tree
from flexlay.property import (
    EnumProperty,
    StringProperty,

)


class DirectionProperty(EnumProperty):

    editable = True

    def __init__(self, label, identifier, default):
        super().__init__(label, identifier, default, optional=True, values=["auto", "left", "right"])


class InlinePosProperty:

    editable = False

    def __init__(self):
        self.identifier = ""  # To stop errors

    def read(self, sexpr, obj):
        obj.pos.x = get_value_from_tree(["x", "_"], sexpr, 0.0)
        obj.pos.y = get_value_from_tree(["y", "_"], sexpr, 0.0)

    def write(self, writer, obj):
        if obj.pos.x != 0:
            writer.write_float("x", obj.pos.x)
        if obj.pos.y != 0:
            writer.write_float("y", obj.pos.y)

    def property_dialog(self, dialog):
        pass


class InlineTilePosProperty(InlinePosProperty):
    """Written to file as coords on tilemap, but displays correctly."""

    editable = False

    def read(self, sexpr, obj):
        obj.pos.x = get_value_from_tree(["x", "_"], sexpr, 0.0) * 32
        obj.pos.y = get_value_from_tree(["y", "_"], sexpr, 0.0) * 32

    def write(self, writer, obj):
        tilemap_position = Point(obj.pos.x // 32, obj.pos.y // 32)
        writer.write_inline_point(tilemap_position)


class InlineRectProperty:

    editable = False

    def __init__(self):
        pass

    def read(self, sexpr, obj):
        obj.pos.x = get_value_from_tree(["x", "_"], sexpr, 0.0)
        obj.pos.y = get_value_from_tree(["y", "_"], sexpr, 0.0)
        obj.size.width = get_value_from_tree(["width", "_"], sexpr, 0.0)
        obj.size.height = get_value_from_tree(["height", "_"], sexpr, 0.0)

    def write(self, writer, obj):
        writer.write_inline_sizef(obj.size)
        writer.write_inline_pointf(obj.pos)

    def property_dialog(self, dialog):
        pass


class SpriteProperty(StringProperty):

    editable = False
    placeholder = "default"

    def write(self, writer, obj):
        if self.value:
            super().write(writer, obj)


class BadGuyProperty(EnumProperty):

    editable = True

    def __init__(self, label, identifier, supertux_gameobj_factory):
        super().__init__(label, identifier, 0, values=[badguy[0] for badguy in supertux_gameobj_factory.badguys])


class ImageProperty(StringProperty):

    editable = False

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)


class SoundProperty(StringProperty):

    editable = False

    def __init__(self, label, identifier, default=""):
        super().__init__(label, identifier, default=default)

    def property_dialog(self, dialog):
        pass


class PathProperty:
    class Node:

        mode_values = ['oneshot', 'pingpong', 'circular']

        def __init__(self, x, y, time):
            self.x = x
            self.y = y
            self.time = time

    editable = False

    def __init__(self, label, identifier):
        self.label = label
        self.identifier = identifier
        self.mode = 2
        self.nodes = []

    def read(self, sexpr, obj):
        self.nodes = []

        sexpr = get_value_from_tree([self.identifier], sexpr, [])
        for node in sexpr:
            if node[0] == 'node':
                x = get_value_from_tree(["x", "_"], node[1:], 0)
                y = get_value_from_tree(["y", "_"], node[1:], 0)
                time = get_value_from_tree(["time", "_"], node[1:], 1)
                self.nodes.append(PathProperty.Node(x, y, time))
            elif node[0] == 'mode':
                if node[1] in PathProperty.Node.mode_values:
                    self.mode = PathProperty.Node.mode_values.index(node[1])
                else:
                    raise RuntimeError("unknown enum value %r" % node[1])
            else:
                raise RuntimeError("unknown tag %r" % node[0])

    def write(self, writer, obj):
        if self.nodes:
            writer.begin_list("path")
            if self.mode != 2:
                writer.write_string("mode", PathProperty.Node.mode_values[self.mode])
            for node in self.nodes:
                writer.begin_list("node")
                writer.write_int("x", node.x)
                writer.write_int("y", node.y)
                if node.time != 1:
                    writer.write_float("time", node.time)
                writer.end_list()
            writer.end_list()

    def property_dialog(self, dialog):
        pass


class SampleProperty(StringProperty):

    editable = False

    def __init__(self, label, identifier, default):
        super().__init__(label, identifier, default, optional=True)


class TilemapProperty(EnumProperty):

    editable = False

    def __init__(self, label, identifier, optional, placeholder=None):
        super().__init__(label, identifier, 0, optional=optional, values=None)
        # super().__init__(label, identifier, "", optional=True, placeholder=placeholder)

    def property_dialog(self, dialog):
        self.values = self._get_tilemaps()
        super().property_dialog(dialog)

    def _get_tilemaps(self):
        sector = Workspace.current.current_sector
        if sector is None:
            return [""]
        return [""] + [tilemap.name for tilemap in sector.tilemaps]


class SectorProperty(StringProperty):

    editable = False

    def __init__(self, label, identifier, default, optional):
        super().__init__(label, identifier, default, optional=optional)


# EOF #
