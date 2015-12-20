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


from flexlay import ObjectLayer, ObjMapTilemapObject, EditorMap
from flexlay.math import Rect
from .gameobj_factor import supertux_gameobj_factory
from .gameobj import Camera
from .tilemap import SuperTuxTileMap


class Sector:
    def __init__(self, parent):
        self.parent = parent
        self.name = None
        self.music = None
        self.gravity = 10.0
        self.init_script = ""
        self.ambient_light = [1, 1, 1]

        self.width = None
        self.height = None

        self.tilemaps = []

        self.object_layer = None
        # self.sketch = None
        self.editormap = None

    def get_some_solid_tilemap(self):
        for tilemap in self.tilemaps:
            if tilemap.solid:
                return tilemap
        return self.tilemaps[0]

    def get_level(self):
        return self.parent

    def resize(self, size, pos):
        self.width = size.width
        self.height = size.height

        for tilemap in self.tilemaps:
            tilemap.resize(size, pos)

        for obj in self.object_layer.objects:
            p = obj.get_pos()
            p += 32 * pos
            obj.set_pos(p)

    def new_from_size(self, name, width, height):
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
            self.object_layer.add_object(ObjMapTilemapObject(tilemap.tilemap_layer, tilemap))
            #camera = Camera()
            #self.object_layer.add_object(camera.objmap_object)
        self.editormap.add_layer(self.object_layer)
        # self.editormap.add_layer(self.sketch)
        self.editormap.metadata = self
        return self

    def load_v2(self, data):
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
                    self.width = max(self.width, tilemap.tilemap_layer.width)
                    self.height = max(self.height, tilemap.tilemap_layer.height)

                self.editormap.set_bounding_rect(Rect(0, 0, self.width * 32, self.height * 32))
            else:
                obj = supertux_gameobj_factory.create_gameobj(name, data)
                if obj is None:
                    print("Error: Couldn't resolve object type: ", name)
                    print("Sector: Unhandled tag: ", name)
                else:
                    self.objects.append(obj)

        # Sort tilemaps according to z-pos before adding them:
        self.tilemaps = sorted(self.tilemaps, key=lambda tilemap: tilemap.z_pos)
        for tilemap in self.tilemaps:
            self.object_layer.add_object(ObjMapTilemapObject(tilemap.tilemap_layer, tilemap))

        for obj in self.objects:
            self.object_layer.add_object(obj.objmap_object)

        self.editormap.metadata = self

    def write(self, writer):
        writer.write_string("name", self.name)
        if self.music:
            writer.write_string("music", self.music)
        if self.init_script:
            writer.write_string("init-script", self.init_script)
        writer.write_rgb("ambient-light", self.ambient_light)

        for obj in self.object_layer.get_objects():
            obj.metadata.write(writer, obj)

            # for tilemap in self.tilemaps:
            #   tilemap.write(writer)

# EOF #
