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
from flexlay import Colorf


class Property:
    '''
    A property is just that: a property
    What these classes do is allow properties to easily be written to files,
    and displayed in dialogs.
    @see: flexlay/gui/generic_dialog.py, supertux/properties_widget.py 
    '''
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

    def on_value_change(self, value):
        self.value = value


class BoolProperty(Property):

    def property_dialog(self, dialog):
        dialog.add_bool(self.label, self.value, self.on_value_change)


class IntProperty(Property):

    def __init__(self, label, identifier, default=0, optional=False):
        super().__init__(label, identifier, default, optional)

    def property_dialog(self, dialog):
        dialog.add_int(self.label, self.value, self.on_value_change)


class FloatProperty(Property):

    def __init__(self, label, identifier, default=0.0, optional=False):
        super().__init__(label, identifier, default, optional)

    def property_dialog(self, dialog):
        dialog.add_int(self.label, self.value, self.on_value_change)


class StringProperty(Property):

    def __init__(self, label, identifier, default="", optional=False, translatable=False):
        super().__init__(label, identifier, default, optional)
        self.translatable = translatable

    def read(self, sexpr, obj):
        self.value = get_value_from_tree([self.identifier, "_"],  sexpr, self.default)

    def write(self, writer, obj):
        if not self.optional or self.value != self.default:
            writer.write_string(self.identifier, self.value, translatable=self.translatable)

    def property_dialog(self, dialog):
        dialog.add_string(self.label, self.value, self.on_value_change)
        
class FileProperty(StringProperty):
    def __init__(self, label, identifier, default="", relative_to="", open_in=""):
        super().__init__(label, identifier, default=default)
        #Where the file dialog opens by default
        self.open_in = open_in
        
        #Where the path shown is relative to (if possible)
        self.relative_to = relative_to
        
        #The actual path stored, so that the relative path can be displayed.
        self.actual_path = ""
        
    def property_dialog(self, dialog):
        dialog.add_file(self)

class EnumProperty(Property):

    def __init__(self, label, identifier, default, optional=False, values=None):
        super().__init__(label, identifier, default, optional=optional)

        if values is None:
            values = []

        self.values = values

    def read(self, sexpr, obj):
        value_name = get_value_from_tree([self.identifier, "_"],  sexpr, self.values[self.default])
        try:
            self.value = self.values.index(value_name)
        except:
            raise RuntimeError("%s: invalid enum value: %r not in %r" % (self.identifier, self.value, self.values))

    def write(self, writer, obj):
        if not self.optional or self.value != self.default:
            writer.write_string(self.identifier, self.values[self.value])

    def property_dialog(self, dialog):
        dialog.add_enum(self.label, self.values, self.value, self.on_value_change)


class DirectionProperty(EnumProperty):

    def __init__(self, label, identifier, default):
        super().__init__(label, identifier, default, optional=True, values=["auto", "left", "right"])


class InlinePosProperty:

    def __init__(self):
        pass

    def read(self, sexpr, obj):
        obj.pos.x = get_value_from_tree(["x", "_"],  sexpr, 0.0)
        obj.pos.y = get_value_from_tree(["y", "_"],  sexpr, 0.0)

    def write(self, writer, obj):
        writer.write_inline_pointf(obj.pos)

    def property_dialog(self, dialog):
        pass


class InlineRectProperty:

    def __init__(self):
        pass

    def read(self, sexpr, obj):
        obj.pos.x = get_value_from_tree(["x", "_"],  sexpr, 0.0)
        obj.pos.y = get_value_from_tree(["y", "_"],  sexpr, 0.0)
        obj.size.width = get_value_from_tree(["width", "_"],  sexpr, 0.0)
        obj.size.height = get_value_from_tree(["height", "_"],  sexpr, 0.0)

    def write(self, writer, obj):
        writer.write_inline_sizef(obj.size)
        writer.write_inline_pointf(obj.pos)

    def property_dialog(self, dialog):
        pass


class SpriteProperty(StringProperty):

    pass


class ImageProperty(StringProperty):

    pass


class ColorProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, Colorf())

    def read(self, sexpr, obj):
        self.value = Colorf(*get_value_from_tree([self.identifier],  sexpr, [1.0, 1.0, 1.0]))

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

    def __init__(self, label, identifier, default):
        super().__init__(label, identifier, default, optional=True)


class TilemapProperty(StringProperty):

    def __init__(self, label, identifier):
        super().__init__(label, identifier, "", optional=True)


class SectorProperty(StringProperty):

    def __init__(self, label, identifier, default, optional):
        super().__init__(label, identifier, default, optional=optional)


# EOF #
