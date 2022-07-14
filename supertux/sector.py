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


from typing import Any, Optional, TYPE_CHECKING

import logging

from flexlay.object_layer import ObjectLayer
from flexlay.objmap_tilemap_object import ObjMapTilemapObject
from flexlay.editor_map import EditorMap
from flexlay.math import Point, Pointf, Size, Rectf
from flexlay.util.sexpr_writer import SExprWriter

from supertux.gameobjs import Camera, SpawnPoint
from supertux.tilemap import SuperTuxTileMap
from supertux.gameobj_factor import supertux_gameobj_factory

if TYPE_CHECKING:
    from supertux.level import Level


class Sector:

    def __init__(self, parent: 'Level') -> None:
        self.parent: Level = parent
        self.name: str = ""
        self.music: str = ""
        self.gravity: float = 10.0
        self.init_script: str = ""
        self.ambient_light: list[float] = [1, 1, 1]

        self.width: int = 0
        self.height: int = 0

        self.tilemaps: list[SuperTuxTileMap] = []
        self.camera: Optional[Camera] = None

        self.object_layer: Optional[ObjectLayer] = None
        # self.sketch = None
        self.editormap: Optional[EditorMap] = None

    def get_some_solid_tilemap(self) -> SuperTuxTileMap:
        for tilemap in self.tilemaps:
            if tilemap.solid:
                return tilemap
        return self.tilemaps[0]

    def get_level(self) -> 'Level':
        return self.parent

    def resize(self, size: Size, pos: Point) -> None:
        self.width = size.width
        self.height = size.height

        for tilemap in self.tilemaps:
            tilemap.resize(size, pos)

        assert self.object_layer is not None
        for obj in self.object_layer.objects:
            p = obj.get_pos() + 32 * pos.to_f()
            obj.set_pos(p)

    def new_from_size(self, name: str, width: int, height: int) -> 'Sector':
        self.name = name
        self.music = ""
        self.gravity = 10.0

        self.width = width
        self.height = height

        self.tilemaps.append(SuperTuxTileMap.from_size(self.width, self.height, "background", -100, False))
        self.tilemaps.append(SuperTuxTileMap.from_size(self.width, self.height, "interactive", 0, True))
        self.tilemaps.append(SuperTuxTileMap.from_size(self.width, self.height, "foreground", 100, False))

        self.object_layer = ObjectLayer()

        self.editormap = EditorMap()
        # self.editormap.set_background_color(Color(255, 255, 255))
        for tilemap in self.tilemaps:
            # self.editormap.add_layer(tilemap.tilemap_layer)
            assert self.object_layer is not None
            assert tilemap.tilemap_layer is not None
            self.object_layer.add_object(ObjMapTilemapObject(tilemap.tilemap_layer, tilemap))

        spawn = SpawnPoint()
        spawn.properties[0].value = "main"
        spawn_x = 5 if self.width > 5 else 0
        spawn_y = self.height * 3 / 4
        assert spawn.objmap_object is not None
        spawn.objmap_object.pos = Pointf(spawn_x * 32, spawn_y * 32)
        assert self.object_layer is not None
        assert spawn.objmap_object is not None
        self.object_layer.add_object(spawn.objmap_object)

        self.camera = Camera()
        assert self.camera.objmap_object is not None
        self.object_layer.add_object(self.camera.objmap_object)

        self.editormap.add_layer(self.object_layer)
        # self.editormap.add_layer(self.sketch)
        self.editormap.metadata = self
        return self

    def load_v2(self, data: list[Any]) -> None:
        self.name = "<No Name>"
        self.music = ""
        self.init_script = ""
        self.gravity = 10.0

        self.width = 0
        self.height = 0

        self.objects = []
        self.tilemaps = []

        self.object_layer = ObjectLayer()
        self.editormap = EditorMap()
        self.editormap.add_layer(self.object_layer)

        for i in data:
            (name, data) = i[0], i[1:]
            if name == "name":
                self.name = data[0]
            elif name == "ambient-light":
                self.ambient_light = data
            elif name == "gravity":
                self.gravity = data[0]
            elif name == "music":
                self.music = data[0]
            elif name == "init-script":
                self.init_script = data[0]
            elif name == "tilemap":
                tilemap = SuperTuxTileMap.from_sexpr(data)
                self.tilemaps.append(tilemap)
                # self.editormap.add_layer(tilemap.tilemap_layer)

                # GRUMBEL: incorrect
                if tilemap.solid:
                    assert tilemap.tilemap_layer is not None
                    self.width = max(self.width, tilemap.tilemap_layer.width)
                    self.height = max(self.height, tilemap.tilemap_layer.height)

                self.editormap.set_bounding_rect(Rectf(0, 0, self.width * 32, self.height * 32))

                assert tilemap.tilemap_layer is not None
                self.object_layer.add_object(ObjMapTilemapObject(tilemap.tilemap_layer, tilemap))
            else:
                obj = supertux_gameobj_factory.create_gameobj(name, data)
                if obj is None:
                    logging.error("Couldn't resolve object type: " + name)
                    logging.warning("Sector: Unhandled tag: " + name)
                else:
                    if isinstance(obj, Camera):
                        self.camera = obj
                    self.objects.append(obj)
                    assert obj.objmap_object is not None
                    self.object_layer.add_object(obj.objmap_object)

        self.editormap.metadata = self

    def write(self, writer: SExprWriter) -> None:
        writer.write_string("name", self.name)
        if self.music:
            writer.write_string("music", self.music)
        if self.gravity != 10.0:
            writer.write_float("gravity", self.gravity)
        if self.init_script:
            writer.write_string("init-script", self.init_script)
        writer.write_rgb("ambient-light", self.ambient_light)

        assert self.object_layer is not None
        for obj in self.object_layer.get_objects():
            obj.metadata.write(writer, obj)

            # for tilemap in self.tilemaps:
            #   tilemap.write(writer)


# EOF #
