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


from typing import Any

from flexlay.util.sexpr_writer import SExprWriter
from flexlay.workspace import Workspace
from flexlay.math import Point
from flexlay.util.sexpr_reader import get_value_from_tree
from flexlay.gui.properties_widget import PropertiesWidget
from flexlay.property import (
    Property,
    EnumProperty,
    StringProperty,
    IntProperty,
)


class DirectionProperty(EnumProperty):

    editable = True

    def __init__(self, label: str, identifier: str, default: Any) -> None:
        super().__init__(label, identifier, default, optional=True, values=["auto", "left", "right"])


class InlinePosProperty(Property):

    editable = False

    def __init__(self) -> None:
        self.identifier = ""  # To stop errors

    def read(self, sexpr: Any, obj: Any) -> None:
        obj.pos.x = get_value_from_tree(["x", "_"], sexpr, 0.0)
        obj.pos.y = get_value_from_tree(["y", "_"], sexpr, 0.0)

    def write(self, writer: SExprWriter, obj: Any) -> None:
        if obj.pos.x != 0:
            writer.write_float("x", obj.pos.x)
        if obj.pos.y != 0:
            writer.write_float("y", obj.pos.y)


class InlineTilePosProperty(InlinePosProperty):
    """Written to file as coords on tilemap, but displays correctly."""

    editable = False

    def read(self, sexpr: Any, obj: Any) -> None:
        obj.pos.x = get_value_from_tree(["x", "_"], sexpr, 0.0) * 32
        obj.pos.y = get_value_from_tree(["y", "_"], sexpr, 0.0) * 32

    def write(self, writer: SExprWriter, obj: Any) -> None:
        tilemap_position = Point(obj.pos.x // 32, obj.pos.y // 32)
        writer.write_inline_point(tilemap_position)


class InlineRectProperty(Property):

    editable = False

    def __init__(self) -> None:
        pass

    def read(self, sexpr: Any, obj: Any) -> None:
        obj.pos.x = get_value_from_tree(["x", "_"], sexpr, 0.0)
        obj.pos.y = get_value_from_tree(["y", "_"], sexpr, 0.0)
        obj.size.width = get_value_from_tree(["width", "_"], sexpr, 0.0)
        obj.size.height = get_value_from_tree(["height", "_"], sexpr, 0.0)

    def write(self, writer: SExprWriter, obj: Any) -> None:
        writer.write_inline_sizef(obj.size)
        writer.write_inline_pointf(obj.pos)


class SpriteProperty(StringProperty):

    editable = False
    placeholder = "default"

    def write(self, writer: SExprWriter, obj: Any) -> None:
        if self.value:
            super().write(writer, obj)


class BadGuyProperty(EnumProperty):

    editable = True

    def __init__(self, label: str, identifier: str) -> None:
        from supertux.gameobj_factor import supertux_gameobj_factory
        super().__init__(label, identifier, 0, values=[badguy[0] for badguy in supertux_gameobj_factory.badguys])


class ImageProperty(StringProperty):

    editable = False

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)


class SoundProperty(StringProperty):

    editable = False

    def __init__(self, label: str, identifier: str, default: str = "") -> None:
        super().__init__(label, identifier, default=default)


class PathProperty(Property):

    class Node:

        mode_values = ['oneshot', 'pingpong', 'circular']

        def __init__(self, x: float, y: float, time: float) -> None:
            self.x = x
            self.y = y
            self.time = time

    editable = False

    def __init__(self, label: str, identifier: str) -> None:
        self.label: str = label
        self.identifier: str = identifier
        self.mode: int = 2
        self.nodes: list[PathProperty.Node] = []

    def read(self, sexpr: Any, obj: Any) -> None:
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

    def write(self, writer: SExprWriter, obj: Any) -> None:
        if self.nodes:
            writer.begin_list("path")
            if self.mode != 2:
                writer.write_string("mode", PathProperty.Node.mode_values[self.mode])
            for node in self.nodes:
                writer.begin_list("node")
                writer.write_float("x", node.x)
                writer.write_float("y", node.y)
                if node.time != 1:
                    writer.write_float("time", node.time)
                writer.end_list()
            writer.end_list()


class SampleProperty(StringProperty):

    editable = False

    def __init__(self, label: str, identifier: str, default: str) -> None:
        super().__init__(label, identifier, default, optional=True)


class TilemapProperty(EnumProperty):

    editable = False

    def __init__(self, label: str, identifier: str, optional: bool, placeholder: Any = None) -> None:
        super().__init__(label, identifier, 0, optional=optional, values=None)
        # super().__init__(label, identifier, "", optional=True, placeholder=placeholder)

    def property_dialog(self, dialog: PropertiesWidget) -> None:
        self.values = self._get_tilemaps()
        super().property_dialog(dialog)

    def _get_tilemaps(self) -> list[str]:
        assert Workspace.current is not None
        sector = Workspace.current.current_sector
        if sector is None:
            return [""]
        return [""] + [tilemap.name for tilemap in sector.tilemaps]


class SectorProperty(StringProperty):

    editable = False

    def __init__(self, label: str, identifier: str, default: Any, optional: bool) -> None:
        super().__init__(label, identifier, default, optional=optional)


class ZPosProperty(IntProperty):

    editable = True

    def __init__(self, default: int = 0) -> None:
        super().__init__("Z-Pos", "z-pos", default=default, optional=True)


# EOF #
