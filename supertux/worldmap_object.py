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


from flexlay import ObjectAddCommand, Sprite, ObjMapSpriteObject, Config
from flexlay.math import Pointf
from flexlay.util import get_value_from_tree


class WorldmapObject:

    def __init__(self):
        self.obj = None


class WMSpawnPoint(WorldmapObject):

    def __init__(self):
        super().__init__()

        self.name = ""
        self.obj = ObjMapSpriteObject(
            Sprite.from_file(Config.current.datadir + "images/worldmap/common/tux.png"),
            Pointf(0, 0), self)
        self.obj.sig_move.connect(self.on_move)

    def on_move(self, data):
        pos = self.obj.get_pos()
        pos.x = int((pos.x + 16) / 32) * 32
        pos.y = int((pos.y + 16) / 32) * 32
        self.obj.set_pos(pos)

    def parse(self, data):
        x = get_value_from_tree(["x", "_"], data, 0)
        y = get_value_from_tree(["y", "_"], data, 0)
        self.obj.set_pos(Pointf(x * 32, y * 32))
        self.name = get_value_from_tree(["name", "_"], data, "")

    def save(self, writer):
        writer.begin_list("spawnpoint")
        pos = self.obj.get_pos()
        writer.write_int("x", pos.x / 32)
        writer.write_int("y", pos.y / 32)
        writer.write_string("name", self.name)
        writer.end_list("spawnpoint")

    def property_dialog(self, gui):
        dialog = gui.create_generic_dialog("SpawnPoint Property Dialog")
        dialog.add_string("Name", self.name)

        def on_callback(name):
            self.name = name

        dialog.set_callback(on_callback)


class WorldmapLevel(WorldmapObject):

    def __init__(self):
        super().__init__()

        self.name = ""
        self.extro_filename = ""
        self.sprite = ""
        self.quit_worldmap = False
        self.obj = ObjMapSpriteObject(
            Sprite.from_file(Config.current.datadir + "images/worldmap/common/leveldot_green.png"),
            Pointf(0, 0), self)
        self.obj.sig_move.connect(self.on_move)

    def parse(self, data):
        x = get_value_from_tree(["x", "_"], data, 0)
        y = get_value_from_tree(["y", "_"], data, 0)
        self.obj.set_pos(Pointf(x * 32, y * 32))
        self.name = get_value_from_tree(["name", "_"], data, "")
        self.sprite = get_value_from_tree(["sprite", "_"], data, "")
        self.extro_filename = get_value_from_tree(["extro-filename", "_"], data, "")
        self.quit_worldmap = get_value_from_tree(["quit-worldmap", "_"], data, False)

    def save(self, writer):
        writer.begin_list("level")
        pos = self.obj.get_pos()
        writer.write_int("x", pos.x / 32)
        writer.write_int("y", pos.y / 32)
        if self.sprite != "":
            writer.write_string("sprite", self.sprite)
        writer.write_string("name", self.name)
        if self.extro_filename != "":
            writer.write_string("extro-filename", self.extro_filename)
        if self.quit_worldmap:
            writer.write_bool("quit-worldmap", self.quit_worldmap)
        writer.end_list("level")

    def on_move(self, data):
        pos = self.obj.get_pos()
        pos.x = int((pos.x + 16) / 32) * 32
        pos.y = int((pos.y + 16) / 32) * 32
        self.obj.set_pos(pos)

    def property_dialog(self, gui):
        dialog = gui.create_generic_dialog("LevelTile Property Dialog")
        dialog.add_string("level", self.name)
        dialog.add_string("sprite", self.sprite)
        dialog.add_string("extro-filename", self.extro_filename)
        dialog.add_bool("quit-worldmap", self.quit_worldmap)

        def on_callback(name, sprite, extro_filename, quit_worldmap):
            self.name = name
            self.sprite = sprite
            self.extro_filename = extro_filename
            self.quit_worldmap = quit_worldmap

        dialog.set_callback(on_callback)


class SpecialTile(WorldmapObject):

    def __init__(self):
        super().__init__()

        self.map_message = ""
        self.apply_to_direction = ""
        self.passive_message = False
        self.teleport_x = 0
        self.teleport_y = 0
        self.invisible_tile = False
        self.obj = ObjMapSpriteObject(
            Sprite.from_file(Config.current.datadir + "images/worldmap/common/teleporterdot.png"),
            Pointf(0, 0), self)
        self.obj.sig_move.connect(self.on_move)

    def parse(self, data):
        x = get_value_from_tree(["x", "_"], data, 0)
        y = get_value_from_tree(["y", "_"], data, 0)
        self.obj.set_pos(Pointf(x * 32, y * 32))
        self.map_message = get_value_from_tree(["map-message", "_"], data, "")
        self.passive_message = get_value_from_tree(["passive-message", "_"], data, False)
        self.teleport_x = get_value_from_tree(["teleport-to-x", "_"], data, -1)
        self.teleport_y = get_value_from_tree(["teleport-to-y", "_"], data, -1)
        self.invisible_tile = get_value_from_tree(["invisible_tile", "_"], data, False)
        self.apply_to_direction = get_value_from_tree(["apply-to-direction", "_"],
                                                      data, "")

    def save(self, writer):
        writer.begin_list("special-tile")
        pos = self.obj.get_pos()
        writer.write_int("x", pos.x / 32)
        writer.write_int("y", pos.y / 32)
        if self.map_message != "":
            writer.write_string("map-message", self.map_message, True)
        if self.passive_message:
            writer.write_bool("passive-message", self.passive_message)
        if self.invisible_tile:
            writer.write_bool("invisible-tile", self.invisible_tile)
        if self.apply_to_direction != "":
            writer.write_string("apply-to-direction", self.apply_to_direction)
        if self.teleport_x != -1:
            writer.write_int("teleport-to-x", self.teleport_x)
            writer.write_int("teleport-to-y", self.teleport_y)
        writer.end_list("special-tile")

    def on_move(self, data):
        pos = self.obj.get_pos()
        pos.x = int((pos.x + 16) / 32) * 32
        pos.y = int((pos.y + 16) / 32) * 32
        self.obj.set_pos(pos)

    def property_dialog(self, gui):
        dialog = gui.create_generic_dialog("SpecialTile Property Dialog")
        dialog.add_string("map-message", self.map_message)
        dialog.add_bool("passive-message", self.passive_message)
        dialog.add_bool("invisible-tile", self.invisible_tile)
        dialog.add_string("apply-to-direction", self.apply_to_direction)
        dialog.add_int("teleport-to-x", self.teleport_x)
        dialog.add_int("teleport-to-y", self.teleport_y)

        def on_callback(map_message, passive_message, invisible_tile,
                        apply_to_direction, teleport_x, teleport_y):
            self.map_message = map_message
            self.passive_message = passive_message
            self.invisible_tile = invisible_tile
            self.apply_to_direction = apply_to_direction
            self.teleport_x = teleport_x
            self.teleport_y = teleport_y

        dialog.set_callback(on_callback)


worldmap_objects = [
    ["level", "images/worldmap/common/leveldot_green.png", WorldmapLevel],
    ["special-tile", "images/worldmap/common/teleporterdot.png", SpecialTile],
    ["spawnpoint", "images/worldmap/common/tux.png", WMSpawnPoint],
]


def create_worldmapobject_at_pos(objmap, name, pos):
    objectclass = [obj for obj in worldmap_objects if obj[0] == name]
    if objectclass is []:
        print("Error: Couldn't resolve object type: ", name)
        return

    name, _, _class = objectclass
    obj = _class()
    obj.obj.set_pos(pos)
    cmd = ObjectAddCommand(objmap)
    cmd.add_object(obj.obj)
    from .gui import SuperTuxGUI
    SuperTuxGUI.current.workspace.get_map().execute(cmd)
    return obj


def create_worldmapobject_from_data(objmap, name, sexpr):
    objectclass = [obj for obj in worldmap_objects if obj[0] == name]
    if objectclass is []:
        print("Error: Couldn't resolve object type: ", name)
        return

    name, _, _class = objectclass
    obj = _class()
    obj.parse(sexpr)
    cmd = ObjectAddCommand(objmap)
    cmd.add_object(obj.obj)
    from .gui import SuperTuxGUI
    SuperTuxGUI.current.workspace.get_map().execute(cmd)
    return obj


# EOF #
