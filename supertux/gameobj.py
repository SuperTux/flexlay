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


from flexlay import (Color, Sprite, Config)
from flexlay.math import Point, Rect, Size
from flexlay.util import get_value_from_tree


class GameObj:

    def __init__(self):
        self.obj = None

    def property_dialog(self, gui):
        print("Object ", type(self), " has no properties")

    def set_obj(self, obj):
        self.obj = obj


class ResetPoint(GameObj):

    def __init__(self):
        super().__init__()

    def save(self, writer, obj):
        pos = obj.get_pos()
        writer.begin_list("firefly")
        writer.write_inline_point(pos)
        writer.end_list()


class Decal(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.layer = get_value_from_tree(["layer", "_"],  sexpr, 0)
        self.sprite = get_value_from_tree(["sprite", "_"], sexpr, "")
        data.set_sprite(Sprite.from_file(Config.current.datadir + self.sprite))

    def save(self, writer, obj):
        writer.begin_list("decal")
        writer.write_inline_point(obj.get_pos())
        writer.write_int("layer", self.layer)
        writer.write_string("sprite", self.sprite)
        writer.end_list()


class SecretArea(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.data = data

        self.message = get_value_from_tree(["message", "_"], sexpr, "")
        self.fade_tilemap = get_value_from_tree(["fade-tilemap", "_"], sexpr, "")

        x = get_value_from_tree(["x", "_"],  sexpr, None)
        y = get_value_from_tree(["y", "_"],  sexpr, None)
        width = get_value_from_tree(["width", "_"],  sexpr, 64)
        height = get_value_from_tree(["height", "_"], sexpr, 64)
        if x is not None and y is not None:
            self.data.set_rect(Rect(Point(x, y),
                                    Size(width, height)))

    def save(self, writer, obj):
        rect = self.data.get_rect()
        writer.begin_list("secretarea")
        writer.write_string("fade-tilemap", self.fade_tilemap)
        writer.write_inline_rect(rect)
        if self.message:
            writer.write_tr_string("message", self.message)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.create_generic_dialog("SecretArea Property Dialog")
        dialog.add_string("Message: ", self.message)

        def on_callback(message):
            self.message = message

        dialog.set_callback(on_callback)


class AmbientSound(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.data = data
        self.data.set_color(Color(200, 200, 200, 128))
        self.factor = get_value_from_tree(["distance_factor", "_"],  sexpr, 0.1)
        self.bias = get_value_from_tree(["distance_bias", "_"],  sexpr, 200)
        self.sample = get_value_from_tree(["sample", "_"],  sexpr, "waterfall")
        self.volume = get_value_from_tree(["volume", "_"],  sexpr, 1)

        x = get_value_from_tree(["x", "_"],  sexpr, None)
        y = get_value_from_tree(["y", "_"],  sexpr, None)
        width = get_value_from_tree(["width", "_"],  sexpr, 64)
        height = get_value_from_tree(["height", "_"], sexpr, 64)
        if x is not None and y is not None:
            self.data.set_rect(Rect(Point(x, y), Size(width, height)))

    def save(self, writer, obj):
        rect = self.data.get_rect()

        writer.begin_list("ambient_sound")
        writer.write_inline_rect(rect)
        writer.write_int("sample", self.sample)
        writer.write_float("distance_factor", self.factor)
        writer.write_float("distance_bias", self.bias)
        writer.write_float("volume", self.volume)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("AmbientSound Property Dialog")
        dialog.add_float("Distance Factor: ", self.factor)
        dialog.add_float("Distance Bias: ", self.bias)
        dialog.add_string("Sample: ", self.sample)
        dialog.add_int("Max Volume: ", self.volume)

        def on_callback(factor, bias, sample, volume):
            self.factor = factor
            self.bias = bias
            self.sample = sample
            self.volume = volume

        dialog.set_callback(on_callback)


class SequenceTrigger(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.data = data
        self.sequence = get_value_from_tree(["sequence", "_"], sexpr, "")
        self.data.set_color(Color(255, 0, 0, 128))

        x = get_value_from_tree(["x", "_"],  sexpr, None)
        y = get_value_from_tree(["y", "_"],  sexpr, None)
        width = get_value_from_tree(["width", "_"],  sexpr, 64)
        height = get_value_from_tree(["height", "_"], sexpr, 64)
        if x is not None and y is not None:
            self.data.set_rect(Rect(Point(x, y), Size(width, height)))

    def save(self, writer, obj):
        rect = self.data.get_rect()
        writer.begin_list("sequencetrigger")
        writer.write_inline_rect(rect)
        writer.write_string("sequence", self.sequence)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("SequenceTrigger Property Dialog")
        dialog.add_string("Sequence: ", self.sequence)

        def on_callback(sequence):
            self.sequence = sequence

        dialog.set_callback(on_callback)


class BadGuy(GameObj):

    def __init__(self, kind):
        super().__init__()
        self.kind = kind
        self.direction = "auto"

    def save(self, writer, obj):
        pos = obj.get_pos()
        writer.begin_list(self.kind)
        writer.write_inline_point(pos)
        if self.direction != "auto":
            writer.write_string("direction", self.direction)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("BadGuy Property Dialog")

        dialog.add_enum("Direction: ", ["left", "right", "auto"], self.direction)

        def on_callback(direction):
            if self.direction != "auto" and direction != "auto" and self.direction != direction:
                self.obj.flip_horizontal()
            self.direction = direction

        dialog.set_callback(on_callback)


class Dispenser(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.data = data
        self.badguy = get_value_from_tree(["badguy", "_"], sexpr, "snowball")
        self.cycle = get_value_from_tree(["cycle", "_"], sexpr, 2)

    def save(self, writer, obj):
        pos = obj.get_pos()
        writer.begin_list("dispenser")
        writer.write_inline_point(pos)
        writer.write_string("badguy", self.badguy)
        writer.write_string("cycle", self.cycle)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Dispenser Property Dialog")
        dialog.add_string("Badguy Type: ", self.badguy)
        dialog.add_int("Cycle Type: ", self.cycle)

        def on_callback(badguy, cycle):
            self.badguy = badguy
            self.cycle = cycle

        dialog.set_callback(on_callback)


class Platform(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.data = data
        self.path = get_value_from_tree(["use_path", "_"], sexpr, "path01")
        self.kind = get_value_from_tree(["type", "_"], sexpr, "flying")

    def save(self, writer, obj):
        pos = obj.get_pos()
        writer.begin_list("platform")
        writer.write_inline_point(pos)
        writer.write_string("use_path", self.path)
        writer.write_string("type", self.kind)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Platform Property Dialog")
        dialog.add_string("Use Path: ", self.path)
        dialog.add_string("Platform Type: ", self.kind)

        def on_callback(path, kind):
            self.path = path
            self.kind = kind
        dialog.set_callback(on_callback)


class SpawnPoint(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.data = data
        self.name = get_value_from_tree(["name", "_"], sexpr, "main")
        data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = ((pos.x + 16) // 32) * 32
        pos.y = ((pos.y + 16) // 32) * 32
        self.data.set_pos(pos)

    def save(self, writer, obj):
        writer.begin_list("spawnpoint")
        writer.write_string("name", self.name)
        writer.write_inline_point(obj.get_pos())
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("SpawnPoint Property Dialog")
        dialog.add_string("Name: ", self.name)

        def on_callback(name):
            self.name = name

        dialog.set_callback(on_callback)


class SimpleObject(GameObj):

    def __init__(self, kind):
        super().__init__()
        self.kind = kind

    def save(self, writer, obj):
        writer.begin_list(self.kind)
        writer.write_inline_point(obj.get_pos())
        writer.end_list()


class SimpleTileObject(GameObj):

    def __init__(self, data, kind, sexpr):
        super().__init__()
        self.kind = kind
        self.data = data
        self.data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = (((pos.x + 16) // 32)) * 32
        pos.y = (((pos.y + 16) // 32)) * 32
        self.data.set_pos(pos)

    def save(self, writer, obj):
        writer.begin_list(self.kind)
        writer.write_inline_point(obj.get_pos())
        writer.end_list()


class InfoBlock(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.data = data
        self.message = get_value_from_tree(["message", "_"], sexpr, "")
        self.data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = (((pos.x + 16) // 32)) * 32
        pos.y = (((pos.y + 16) // 32)) * 32
        self.data.set_pos(pos)

    def save(self, writer, obj):
        writer.begin_list("infoblock")
        writer.write_tr_string("message", self.message)
        writer.write_inline_point(obj.get_pos())
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("InfoBlock Property Dialog")
        dialog.add_string("Message: ", self.message)

        def on_callback(message):
            self.message = message

        dialog.set_callback(on_callback)


class Powerup(GameObj):

    def __init__(self, data, sexpr):
        super().__init__()
        self.data = data
        self.sprite = get_value_from_tree(["sprite", "_"], sexpr, "egg")
        self.data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = (((pos.x + 16) // 32)) * 32
        pos.y = (((pos.y + 16) // 32)) * 32
        self.data.set_pos(pos)

    def save(self, writer, obj):
        writer.begin_list("powerup")
        writer.write_inline_point(obj.get_pos())
        writer.write_string("sprite", self.sprite)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Powerup Property Dialog" % self.sprite)
        dialog.add_string("Sprite: ", self.sprite)

        def on_callback(sprite):
            self.sprite = sprite

        dialog.set_callback(on_callback)


class ParticleSystem(GameObj):

    def __init__(self, kind, sexpr):
        super().__init__()
        self.kind = kind
        self.layer = get_value_from_tree(["layer", "_"], sexpr, -1)

    def save(self, writer, obj):
        writer.begin_list("particles")
        if self.layer != -1:
            writer.write_int("layer", self.layer)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("%s-ParticleSystem Property Dialog" % self.kind)
        dialog.add_int("Layer: ", self.layer)

        def on_callback(layer):
            self.layer = layer

        dialog.set_callback(on_callback)


class Gradient(GameObj):

    def __init__(self, obj, sexpr):
        super().__init__()
        self.obj = obj
        self.layer = get_value_from_tree(["layer", "_"], sexpr, -1)
        self.color_top = [0, 0, 0]
        self.color_bottom = [0, 0, 0]
        self.kind = "image"
        if get_value_from_tree(["top_color"], sexpr, []) != []:
            self.color_top = self.parse_color(
                get_value_from_tree(["top_color"], sexpr, []))
            self.color_bottom = self.parse_color(
                get_value_from_tree(["bottom_color"], sexpr, []))

    def parse_color(self, sexpr):
        if len(sexpr) < 3:
            return [0, 0, 0]

        return [sexpr[0], sexpr[1], sexpr[2]]

    def save(self, writer, obj):
        writer.begin_list("gradient")
        writer.write_rgb("top_color", self.color_top)
        writer.write_rgb("bottom_color", self.color_bottom)
        if self.layer != -1:
            writer.write_int("layer", self.layer)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Background Property Dialog")
        dialog.add_int("Layer: ", self.layer)
        dialog.add_int("Top Red: ", self.color_top[0])
        dialog.add_int("Top Green: ", self.color_top[1])
        dialog.add_int("Top Blue: ", self.color_top[2])
        dialog.add_int("Bottom Red: ", self.color_bottom[0])
        dialog.add_int("Bottom Green: ", self.color_bottom[1])
        dialog.add_int("Bottom Blue: ", self.color_bottom[2])

        def on_callback(layer, topred, topgreen, topblue, botred, botgreen, botblue):
            self.layer = layer
            self.color_top[0] = topred
            self.color_top[1] = topgreen
            self.color_top[2] = topblue
            self.color_bottom[0] = botred
            self.color_bottom[1] = botgreen
            self.color_bottom[2] = botblue

        dialog.set_callback(on_callback)


class Background(GameObj):

    def __init__(self, obj, sexpr):
        super().__init__()
        self.obj = obj
        self.image = get_value_from_tree(["image", "_"], sexpr, "")
        self.speed = get_value_from_tree(["speed", "_"], sexpr, 1.0)
        self.layer = get_value_from_tree(["layer", "_"], sexpr, -1)
        self.kind = "image"

    def save(self, writer, obj):
        writer.begin_list("background")
        writer.write_string("image", self.image)
        writer.write_float("speed", self.speed)
        if self.layer != -1:
            writer.write_int("layer", self.layer)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Background Property Dialog")
        dialog.add_string("Image: ", self.image)
        dialog.add_float("Speed: ", self.speed)
        dialog.add_int("Layer: ", self.layer)

        def on_callback(kind, image, speed, layer):
            self.kind = kind
            self.image = image
            self.speed = speed
            self.layer = layer

        dialog.set_callback(on_callback)


class UnimplementedObject(GameObj):

    def __init__(self, sexpr):
        super().__init__()
        self.sexpr = sexpr

    def save(self, f):
        f.write("           (sexpr %s)\n" % self.sexpr)
        # TODO


class LevelTime(GameObj):

    def __init__(self, sexpr):
        super().__init__()
        self.time = get_value_from_tree(["time", "_"], sexpr, 999)

    def save(self, writer, obj):
        writer.begin_list("leveltime")
        writer.write_int("time", self.time)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("LevelTime Property Dialog")
        dialog.add_float("Time: ", self.time)

        def on_callback(time):
            self.time = time

        dialog.set_callback(on_callback)


class Door(GameObj):

    def __init__(self, kind, data, sexpr):
        super().__init__()
        self.kind = kind
        self.data = data
        self.sector = get_value_from_tree(["sector", "_"], sexpr, "main")
        self.spawnpoint = get_value_from_tree(["spawnpoint", "_"], sexpr, "main")

        self.data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = (((pos.x + 16) // 32)) * 32
        pos.y = (((pos.y + 16) // 32)) * 32
        self.data.set_pos(pos)

    def save(self, writer, obj):
        pos = obj.get_pos()
        writer.begin_list(self.kind)
        writer.write_inline_point(pos)
        writer.write_string("sector", self.sector)
        writer.write_string("spawnpoint", self.spawnpoint)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Door Property Dialog")
        dialog.add_string("Sector: ", self.sector)
        dialog.add_string("Spawnpoint: ", self.spawnpoint)

        def on_callback(sector, spawnpoint):
            self.sector = sector
            self.spawnpoint = spawnpoint

        dialog.set_callback(on_callback)


class PathNode(GameObj):

    def __init__(self, node):
        super().__init__()
        self.node = node

    def save(self, writer, obj):
        pass


class ScriptedObject(GameObj):

    def __init__(self, obj, sexpr):
        super().__init__()
        self.obj = obj
        self.name = get_value_from_tree(["name", "_"], sexpr, "")
        self.sprite = get_value_from_tree(["sprite", "_"], sexpr, "")
        self.visible = get_value_from_tree(["visible", "_"], sexpr, True)
        self.physic_enabled = get_value_from_tree(["physic-enabled", "_"], sexpr, False)
        self.solid = get_value_from_tree(["solid", "_"], sexpr, False)
        self.layer = get_value_from_tree(["layer", "_"], sexpr, 100)
        self.load_sprite()

    def save(self, writer, obj):
        pos = obj.get_pos()
        writer.begin_list("scriptedobject")
        writer.write_inline_point(pos)
        writer.write_string("name", self.name)
        writer.write_string("sprite", self.sprite)
        writer.write_int("layer", self.layer)
        writer.write_bool("visible", self.visible)
        writer.write_bool("physic-enabled", self.physic_enabled)
        writer.write_bool("solid", self.solid)
        writer.end_list()

    def load_sprite(self):
        sprite = Sprite.from_file(Config.current.datadir + self.sprite)
        self.obj.set_sprite(sprite)

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Scripted Object Property Dialog")
        dialog.add_string("Name: ", self.name)
        dialog.add_string("Sprite: ", self.sprite)
        dialog.add_int("Layer: ", self.layer)
        dialog.add_bool("Visible: ", self.visible)
        dialog.add_bool("Physics: ", self.physic_enabled)
        dialog.add_bool("Solid: ", self.solid)

        def on_callback(name, sprite, layer, visible, physic_enabled, solid):
            self.name = name
            self.sprite = sprite
            self.layer = layer
            self.visible = visible
            self.physic_enabled = physic_enabled
            self.solid = solid
            self.load_sprite()

        dialog.set_callback(on_callback)


# EOF #
