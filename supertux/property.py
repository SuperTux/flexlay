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


from flexlay.util import get_value_from_tree
from flexlay import Color


class Property:

    def __init__(self, label, identifier, default, optional=False):
        self.label = label
        self.identifier = identifier
        self.value = default
        self.default = default
        self.optional = optional

    def read(self, sexpr, obj):
        self.value = get_value_from_tree([self.identifier, "_"],  sexpr, self.default)

    def write(self, writer, obj):
        if not self.optional or self.value != self.default:
            writer.write(self.identifier, self.value)

    def property_dialog(self, dialog):
        pass


class BoolProperty(Property):

    def property_dialog(self, dialog):
        dialog.add_int(self.label, self.value)


class IntProperty(Property):

    def __init__(self, label, identifier, default=0, optional=False):
        super().__init__(label, identifier, default, optional)

    def property_dialog(self, dialog):
        dialog.add_int(self.label, self.value)


class FloatProperty(Property):

    def __init__(self, label, identifier, default=0.0, optional=False):
        super().__init__(label, identifier, default, optional)

    def property_dialog(self, dialog):
        dialog.add_int(self.label, self.value)


class StringProperty(Property):

    def __init__(self, label, identifier, default="", optional=False):
        super().__init__(label, identifier, default, optional)

    def read(self, sexpr, obj):
        self.value = get_value_from_tree([self.identifier, "_"],  sexpr, self.default)

    def write(self, writer, obj):
        if not self.optional or self.value != self.default:
            writer.write_string(self.identifier, self.value)

    def property_dialog(self, dialog):
        dialog.add_string(self.label, self.value)


class EnumProperty(Property):

    def __init__(self, label, identifier, default, values):
        super().__init__(label, identifier, default, optional=True)
        self.values = values

    def read(self, sexpr, obj):
        self.value = get_value_from_tree([self.identifier, "_"],  sexpr, self.default)
        if self.value not in self.values:
            raise RuntimeError("%s: invalid enum value: %r not in %r" % (self.identifier, self.value, self.values))

    def write(self, writer, obj):
        if not self.optional or self.value != self.default:
            writer.write_string(self.identifier, self.value)

    def property_dialog(self, dialog):
        dialog.add_enum(self.label, self.values, self.value)


class DirectionProperty(EnumProperty):

    def __init__(self, label, identifier, default):
        super().__init__(label, identifier, default, ["left", "right", "auto"])


class PosProperty(Property):

    def property_dialog(self, dialog):
        dialog.add_int(self.label, self.value)


class InlinePosProperty(Property):

    def __init__(self):
        pass

    def read(self, sexpr, obj):
        obj.pos.x = get_value_from_tree(["x", "_"],  sexpr, 0.0)
        obj.pos.y = get_value_from_tree(["y", "_"],  sexpr, 0.0)

    def write(self, writer, obj):
        writer.write_inline_point(obj.pos)

    def property_dialog(self, dialog):
        pass


class InlineRectProperty(Property):

    def __init__(self):
        pass

    def read(self, sexpr, obj):
        obj.pos.x = get_value_from_tree(["x", "_"],  sexpr, 0.0)
        obj.pos.y = get_value_from_tree(["y", "_"],  sexpr, 0.0)
        obj.size.width = get_value_from_tree(["width", "_"],  sexpr, 0.0)
        obj.size.height = get_value_from_tree(["height", "_"],  sexpr, 0.0)

    def write(self, writer, obj):
        writer.write_inline_size(obj.size)
        writer.write_inline_point(obj.pos)


class SpriteProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, "")


class ImageProperty(StringProperty):

    pass


class ColorProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, Color())

    def read(self, sexpr, obj):
        self.value = Color(*get_value_from_tree([self.identifier],  sexpr, Color()))

    def write(self, writer, obj):
        writer.write_vector(self.identifier, self.value.to_list()[0:3])

    def property_dialog(self, dialog):
        pass


class PathProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, "", optional=True)


class SampleProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, "", optional=True)


class TilemapProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, "", optional=True)


# EOF #
