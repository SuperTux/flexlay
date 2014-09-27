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


from flexlay.math import Point
from flexlay.util import get_value_from_tree


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
        pass

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

    def write(self, writer, obj):
        pass


class EnumProperty(Property):

    def __init__(self, label, identifier, default, values):
        super().__init__(label, identifier, default)
        self.values = values


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
        writer.write_inline_point(obj.pos)
        writer.write_inline_size(obj.size)


class SpriteProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, None)


class ImageProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, None)


class ColorProperty(StringProperty):

    pass


class PathProperty(StringProperty):

    pass


class SampleProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, None)


class TilemapProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, None)

    def read(self, sexpr, obj):
        pass

    def write(self, writer, obj):
        pass


# EOF #
