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

from flexlay import Colorf, Workspace
from flexlay.util import get_value_from_tree
from flexlay.math import Point

class Property:
    """
    A property is just that: a property
    What these classes do is allow properties to easily be written to files,
    and displayed in dialogs.
    @see: flexlay/gui/generic_dialog.py, supertux/properties_widget.py
    """
    # Editable means appears in GenericDialog
    editable = False

    def __init__(self, label, identifier, default, optional=False):
        self.label = label
        self.identifier = identifier
        self.value = default
        self.default = default
        self.optional = optional

    def read(self, sexpr, obj):
        self.value = get_value_from_tree([self.identifier, "_"], sexpr, self.default)

    def write(self, writer, obj):
        if not self.optional or self.value != self.default:
            writer.write(self.identifier, self.value)

    def property_dialog(self, dialog):
        pass

    def on_value_change(self, value):
        self.value = value


class BoolProperty(Property):
    editable = True
    def property_dialog(self, dialog):
        dialog.add_bool(self.label, self.value, self.on_value_change)


class IntProperty(Property):
    editable = True

    def __init__(self, label, identifier, default=0, optional=False):
        super().__init__(label, identifier, default, optional)

    def property_dialog(self, dialog):
        dialog.add_int(self.label, self.value, self.on_value_change)


class FloatProperty(Property):
    editable = True

    def __init__(self, label, identifier, default=0.0, optional=False):
        super().__init__(label, identifier, default, optional)

    def property_dialog(self, dialog):
        dialog.add_int(self.label, self.value, self.on_value_change)


class StringProperty(Property):
    editable = True
    placeholder = None

    def __init__(self, label, identifier, default="", optional=False, translatable=False, placeholder=None):
        super().__init__(label, identifier, default, optional)
        self.translatable = translatable

        if placeholder != None:
            self.placeholder = placeholder

    def read(self, sexpr, obj):
        self.value = get_value_from_tree([self.identifier, "_"], sexpr, self.default)

    def write(self, writer, obj):
        if not self.optional or self.value != self.default:
            writer.write_string(self.identifier, self.value, translatable=self.translatable)

    def property_dialog(self, dialog):
        dialog.add_string(self.label, self.value, self.on_value_change, self.placeholder)


class FileProperty(StringProperty):
    editable = True

    def __init__(self, label, identifier, default="", relative_to="", open_in=""):
        """
        :param relative_to: The prefix text not displayed in the input box
        :param open_in: Where the browse dialog opens
        """
        super().__init__(label, identifier, default=default)
        # Where the file dialog opens by default
        self.open_in = open_in

        # Where the path shown is relative to (if possible)
        self.relative_to = relative_to

        # The actual path stored, so that the relative path can be displayed.
        self.actual_path = ""

    def property_dialog(self, dialog):
        dialog.add_file(self.label, self.default, self.relative_to, self.open_in, self.on_value_change)


class EnumProperty(StringProperty):
    editable = True

    def __init__(self, label, identifier, default, optional=False, values=None):
        """
        :param default: Is an index from values!!!
        """
        super().__init__(label, identifier, values[default] if values else "", optional=optional)

        self.default_index = default

        if values is None:
            values = []

        self.values = values

    def property_dialog(self, dialog):
        dialog.add_enum(self.label, self.values, self.values.index(self.value), self.on_value_change)


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
        writer.write_inline_pointf(obj.pos)

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
    pass


class SoundProperty(StringProperty):
    editable = False

    def __init__(self, label, identifier, default=""):
        super().__init__(label, identifier, default=default)

    def property_dialog(self, dialog):
        pass


class ColorProperty(StringProperty):
    editable = True

    def __init__(self, label, identifier):
        super().__init__(label, identifier, Colorf())

    def read(self, sexpr, obj):
        self.value = Colorf(*get_value_from_tree([self.identifier], sexpr, [1.0, 1.0, 1.0]))

    def write(self, writer, obj):
        writer.write_color(self.identifier, self.value.to_list()[0:3])

    def property_dialog(self, dialog):
        dialog.add_color(self.label, self.value)


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

    def get_tilemaps(self):
        sector = Workspace.current.current_sector
        if sector == None:
            return []
        return [tilemap.name for tilemap in sector.tilemaps]

    def __init__(self, label, identifier, placeholder=None):
        super().__init__(label, identifier, 0, values=self.get_tilemaps())
        #super().__init__(label, identifier, "", optional=True, placeholder=placeholder)

    def property_dialog(self, dialog):
        self.values = self.get_tilemaps()
        super().property_dialog(dialog)


class SectorProperty(StringProperty):
    editable = False

    def __init__(self, label, identifier, default, optional):
        super().__init__(label, identifier, default, optional=optional)

# EOF #
