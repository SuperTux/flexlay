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


from typing import cast, Any, Optional

from flexlay.tilemap_layer import TilemapLayer
from flexlay.field import Field
from flexlay.math import Point, Pointf, Size, Rectf
from flexlay.graphic_context import GraphicContext
from flexlay.util.sexpr_reader import get_value_from_tree
from flexlay.util.sexpr_writer import SExprWriter
from flexlay.objmap_tilemap_object import ObjMapTilemapObject

from supertux.tileset import SuperTuxTileset


class SuperTuxTileMap:

    @staticmethod
    def from_sexpr(data: Any) -> 'SuperTuxTileMap':
        # Load position from path, node and then x, ys
        x = 0
        y = 0
        path = get_value_from_tree(["path"], data, None)
        if path:
            node = get_value_from_tree(["node"], path, None)
            if node:
                x = get_value_from_tree(["x", "_"], node, None)
                y = get_value_from_tree(["y", "_"], node, None)

        name = get_value_from_tree(["name", "_"], data, "")
        z_pos = get_value_from_tree(["z-pos", "_"], data, 0)
        solid = get_value_from_tree(["solid", "_"], data, False)

        result = SuperTuxTileMap(z_pos, solid)

        width = get_value_from_tree(["width", "_"], data, 20)
        height = get_value_from_tree(["height", "_"], data, 15)
        result.draw_target = get_value_from_tree(["draw-target", "_"], data, "")
        result.speed = get_value_from_tree(["speed", "_"], data, 1.0)
        result.speed_y = get_value_from_tree(["speed-y", "_"], data, 1.0)
        result.alpha = get_value_from_tree(["alpha", "_"], data, 1.0)

        assert SuperTuxTileset.current is not None
        result.tilemap_layer = TilemapLayer(SuperTuxTileset.current, width, height)
        result.tilemap_layer.set_data(get_value_from_tree(["tiles"], data, []))
        result.tilemap_layer.metadata = result
        result.pos = Point(x, y)
        result.tilemap_layer.name = name

        return result

    @staticmethod
    def from_size(width: int, height: int, name: str, z_pos: float = 0, solid: bool = False) -> 'SuperTuxTileMap':
        result = SuperTuxTileMap(z_pos, solid)
        assert SuperTuxTileset.current is not None
        result.tilemap_layer = TilemapLayer(SuperTuxTileset.current, width, height)
        result.tilemap_layer.metadata = result
        result.tilemap_layer.name = name
        return result

    def __init__(self, z_pos: float = 0, solid: bool = False) -> None:
        self.solid: bool = solid
        self.draw_target: str = ""
        self.z_pos: float = z_pos
        self.speed: float = 1.0
        self.speed_y: float = 1.0
        self.alpha: float = 1.0
        self.pos: Point = Point(0, 0)
        self.tilemap_layer: Optional[TilemapLayer] = None

    @property
    def name(self) -> str:
        assert self.tilemap_layer is not None
        return self.tilemap_layer.name

    def write(self, writer: SExprWriter, objmap_tilemap_object: ObjMapTilemapObject) -> None:
        assert self.tilemap_layer is not None
        writer.begin_list("tilemap")
        writer.write_bool("solid", self.solid)
        if self.draw_target:
            writer.write_string("draw-target", self.draw_target)
        if self.speed != 1.0:
            writer.write_float("speed", self.speed)
        if self.speed_y != 1.0:
            writer.write_float("speed-y", self.speed_y)
        if self.alpha != 1.0:
            writer.write_float("alpha", self.alpha)
        writer.write_float("z-pos", self.z_pos)
        if self.tilemap_layer.name:
            writer.write_string("name", self.tilemap_layer.name)
        if self.pos and (self.pos.x != 0 or self.pos.y != 0):
            writer.begin_list("path")
            writer.begin_list("node")
            writer.write_int("x", self.pos.x)
            writer.write_int("y", self.pos.y)
            writer.end_list("node")
            writer.end_list("path")
        writer.write_int("width", self.tilemap_layer.width)
        writer.write_int("height", self.tilemap_layer.height)
        writer.write_field("tiles", self.tilemap_layer.field)
        writer.end_list()

    def get_bounding_rect(self) -> Rectf:
        assert self.tilemap_layer is not None
        return self.tilemap_layer.get_bounding_rect()

    def has_bounding_rect(self) -> bool:
        assert self.tilemap_layer is not None
        return self.tilemap_layer.has_bounding_rect()

    def draw(self, gc: GraphicContext) -> None:
        assert self.tilemap_layer is not None
        self.tilemap_layer.draw(gc, self.pos)

    def world2tile(self, p: Pointf) -> Point:
        assert self.tilemap_layer is not None
        return self.tilemap_layer.world2tile(p)

    def get_tileset(self) -> SuperTuxTileset:
        assert self.tilemap_layer is not None
        assert isinstance(self.tilemap_layer.get_tileset(), SuperTuxTileset)
        return cast(SuperTuxTileset, self.tilemap_layer.get_tileset())

    def get_data(self) -> list[int]:
        assert self.tilemap_layer is not None
        return self.tilemap_layer.get_data()

    def set_data(self, data: list[int]) -> None:
        assert self.tilemap_layer is not None
        self.tilemap_layer.set_data(data)

    def resize(self, size: Size, pos: Point) -> None:
        assert self.tilemap_layer is not None
        self.tilemap_layer.resize(size, pos)

    @property
    def field(self) -> Field:
        assert self.tilemap_layer is not None
        return self.tilemap_layer.field

    @property
    def width(self) -> int:
        assert self.tilemap_layer is not None
        return self.tilemap_layer.width

    @property
    def height(self) -> int:
        assert self.tilemap_layer is not None
        return self.tilemap_layer.height


# EOF #
