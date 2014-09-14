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


from flexlay import (Color, Sprite)
from flexlay.math import Point, Rect, Size
from flexlay.util import get_value_from_tree

from .config import Config


class GameObj:

    def __init__(self):
        pass

    def property_dialog(self, gui):
        print("Object ", type(self), " has no properties")

    def set_obj(self, obj):
        self.obj = obj


class SecretArea(GameObj):

    def __init__(self, data, sexpr=[]):
        self.data = data

        self.message = get_value_from_tree(["message", "_"], sexpr, "")

        x = get_value_from_tree(["x", "_"],  sexpr, None)
        y = get_value_from_tree(["y", "_"],  sexpr, None)
        width = get_value_from_tree(["width", "_"],  sexpr, 64)
        height = get_value_from_tree(["height", "_"], sexpr, 64)
        if x is not None and y is not None:
            self.data.set_rect(Rect(Point(x, y),
                                    Size(width, height)))

    def save(self, f, obj):
        rect = self.data.get_rect()
        f.write(("        (secretarea (x %s)\n"
                 "                    (y %s)\n"
                 "                    (width %s)\n"
                 "                    (height %s)\n"
                 "                    (message %r))\n") %
                (rect.left, rect.top, rect.get_width(), rect.get_height(), self.message))

    def property_dialog(self, gui):
        print(self.message)
        dialog = gui.create_generic_dialog("SecretArea Property Dialog")
        dialog.add_string("Message: ", self.message)

        def on_callback(message):
            self.message = message

        dialog.set_callback(on_callback)


class AmbientSound(GameObj):

    def __init__(self, data, sexpr=[]):
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

    def save(self, f, obj):
        rect = self.data.get_rect()
        f.write(("        (ambient_sound (x %s)\n"
                 "                       (y %s)\n"
                 "                       (width %s)\n"
                 "                       (height %s)\n"
                 "                       (sample %s)\n"
                 "                       (distance_factor %s)\n"
                 "                       (distance_bias %s)\n"
                 "                       (volume %s))\n") %
                (rect.left, rect.top, rect.get_width(), rect.get_height(),
                 self.sample, self.factor, self.bias, self.volume))

    def property_dialog(self, gui):
        print(self.factor)
        print(self.bias)
        print(self.sample)
        print(self.volume)
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

    def __init__(self, data, sexpr=[]):
        self.data = data
        self.sequence = get_value_from_tree(["sequence", "_"], sexpr, "")
        self.data.set_color(Color(255, 0, 0, 128))

        x = get_value_from_tree(["x", "_"],  sexpr, None)
        y = get_value_from_tree(["y", "_"],  sexpr, None)
        width = get_value_from_tree(["width", "_"],  sexpr, 64)
        height = get_value_from_tree(["height", "_"], sexpr, 64)
        if x is not None and y is not None:
            self.data.set_rect(Rect(Point(x, y), Size(width, height)))

    def save(self, f, obj):
        rect = self.data.get_rect()
        f.write(("        (sequencetrigger (x %s)\n" +
                 "                         (y %s)\n" +
                 "                         (width %s)\n" +
                 "                         (height %s)\n" +
                 "                         (sequence %s))\n") %
                (rect.left, rect.top, rect.get_width(), rect.get_height(),
                 self.sequence))

    def property_dialog(self, gui):
        print(self.sequence.inspect)
        dialog = gui.gui.create_generic_dialog("SequenceTrigger Property Dialog")
        dialog.add_string("Sequence: ", self.sequence)

        def on_callback(sequence):
            self.sequence = sequence

        dialog.set_callback(on_callback)


class BadGuy(GameObj):

    def __init__(self, type):
        self.type = type
        self.direction = "auto"

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("       (%s (x %d) (y %d) (direction \"%s\")\n" % [self.type, pos.x, pos.y, self.direction])

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("BadGuy Property Dialog")

        dialog.add_enum("Direction: ", ["left", "right", "auto"], self.direction)

        def on_callback(direction):
            if self.direction != "auto" and direction != "auto" and self.direction != direction:
                self.obj.flip_horizontal()
            self.direction = direction

        dialog.set_callback(on_callback)


class Dispenser(GameObj):

    def __init__(self, data, sexpr=[]):
        self.data = data
        self.badguy = get_value_from_tree(["badguy", "_"], sexpr, "snowball")
        self.cycle = get_value_from_tree(["cycle", "_"], sexpr, 2)

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("       (dispenser (x %d) (y %d) (badguy \"%s\") (cycle %d))\n" %
                [pos.x, pos.y, self.badguy, self.cycle])

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Dispenser Property Dialog")
        dialog.add_string("Badguy Type: ", self.badguy)
        dialog.add_int("Cycle Type: ", self.cycle)

        def on_callback(badguy, cycle):
            self.badguy = badguy
            self.cycle = cycle

        dialog.set_callback(on_callback)


class Platform(GameObj):

    def __init__(self, data, sexpr=[]):
        self.data = data
        self.path = get_value_from_tree(["use_path", "_"], sexpr, "path01")
        self.type = get_value_from_tree(["type", "_"], sexpr, "flying")

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("       (platform (x %d) (y %d) (use_path \"%s\") (type \"%s\"))\n" %
                [pos.x, pos.y, self.path, self.type])

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Platform Property Dialog")
        dialog.add_string("Use Path: ", self.path)
        dialog.add_string("Platform Type: ", self.type)

        def on_callback(path, type):
            self.path = path
            self.type = type
        dialog.set_callback(on_callback)


class SpawnPoint(GameObj):

    def __init__(self, data, sexpr=[]):
        self.data = data
        self.name = get_value_from_tree(["name", "_"],  sexpr, "main")
        data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = (((pos.x + 16) // 32)) * 32
        pos.y = (((pos.y + 16) // 32)) * 32
        self.data.set_pos(pos)

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("       (spawnpoint (name \"%s\") (x %d) (y %d))\n" % [self.name, pos.x, pos.y])

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("SpawnPoint Property Dialog")
        dialog.add_string("Name: ", self.name)

        def on_callback(name):
            self.name = name

        dialog.set_callback(on_callback)


class SimpleObject(GameObj):

    def __init__(self, type):
        self.type = type

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("       (%s (x %d) (y %d))\n" % [self.type, pos.x, pos.y])


class SimpleTileObject(GameObj):

    def __init__(self, data, type, sexpr=[]):
        self.type = type
        self.data = data
        self.data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = (((pos.x + 16) // 32)) * 32
        pos.y = (((pos.y + 16) // 32)) * 32
        self.data.set_pos(pos)

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("       (%s (x %d) (y %d))\n" % [self.type, pos.x, pos.y])


class InfoBlock(GameObj):

    def __init__(self, data, sexpr=[]):
        self.data = data
        self.message = get_value_from_tree(["message", "_"], sexpr, "")
        self.data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = (((pos.x + 16) // 32)) * 32
        pos.y = (((pos.y + 16) // 32)) * 32
        self.data.set_pos(pos)

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("      (infoblock (x %d) (y %d)\n" % [pos.x, pos.y])
        f.write("        (message (_ \"%s\"))\n" % [self.message])
        f.write("      )\n")

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("InfoBox Property Dialog" % [self.type])
        dialog.add_string("Message: ", self.message)

        def on_callback(message):
            self.message = message

        dialog.set_callback(on_callback)


class Powerup(GameObj):

    def __init__(self, data, sexpr=[]):
        self.data = data
        self.sprite = get_value_from_tree(["sprite", "_"], sexpr, "egg")
        self.data.sig_move.connect(self.on_move)
        self.on_move(data)

    def on_move(self, data):
        pos = self.data.get_pos()
        pos.x = (((pos.x + 16) // 32)) * 32
        pos.y = (((pos.y + 16) // 32)) * 32
        self.data.set_pos(pos)

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("      (powerup (x %d) (y %d)\n" % [pos.x, pos.y])
        f.write("        (sprite (_ \"%s\"))\n" % [self.sprite])
        f.write("      )\n")

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Powerup Property Dialog" % [self.sprite])
        dialog.add_string("Sprite: ", self.sprite)

        def on_callback(sprite):
            self.sprite = sprite

        dialog.set_callback(on_callback)


class ParticleSystem(GameObj):

    def __init__(self, type, sexpr=[]):
        self.type = type
        self.layer = get_value_from_tree(["layer", "_"], sexpr, -1)

    def save(self, f, obj):
        f.write("       (particles-%s\n" % [self.type])
        if self.layer != -1:
            f.write("         (layer %d)\n" % [self.layer])
        f.write("       )\n")

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("%s-ParticleSystem Property Dialog" % [self.type])
        dialog.add_int("Layer: ", self.layer)

        def on_callback(layer):
            self.layer = layer

        dialog.set_callback(on_callback)


class Gradient(GameObj):

    def __init__(self, object, sexpr=[]):
        self.object = object
        self.layer = get_value_from_tree(["layer", "_"], sexpr, -1)
        self.color_top = [0, 0, 0]
        self.color_bottom = [0, 0, 0]
        self.type = "image"
        if get_value_from_tree(["top_color"], sexpr, []) != []:
            self.color_top = self.parse_color(
                get_value_from_tree(["top_color"], sexpr, []))
            self.color_bottom = self.parse_color(
                get_value_from_tree(["bottom_color"], sexpr, []))

    def parse_color(self, sexpr=[]):
        if sexpr.size() < 3:
            return [0, 0, 0]

        return [sexpr[0], sexpr[1], sexpr[2]]

    def save(self, f, obj):
        f.write("       (gradient\n")
        f.write("         (top_color %f %f %f)\n" % [self.color_top[0], self.color_top[1], self.color_top[2]])
        f.write("         (bottom_color %f %f %f)\n" %
                [self.color_bottom[0], self.color_bottom[1], self.color_bottom[2]])
        if self.layer != -1:
            f.write("         (layer %d)\n" % [self.layer])
        f.write("       )\n")

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

    def __init__(self, object, sexpr=[]):
        self.object = object
        self.image = get_value_from_tree(["image", "_"], sexpr, "")
        self.speed = get_value_from_tree(["speed", "_"], sexpr, 1.0)
        self.layer = get_value_from_tree(["layer", "_"], sexpr, -1)
        self.type = "image"

    def save(self, f, obj):
        f.write("       (background\n")
        f.write("         (image \"%s\")\n" % [self.image])
        f.write("         (speed %f)\n" % [self.speed])
        if self.layer != -1:
            f.write("         (layer %d)\n" % [self.layer])
        f.write("       )\n")

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("Background Property Dialog")
        dialog.add_string("Image: ", self.image)
        dialog.add_float("Speed: ", self.speed)
        dialog.add_int("Layer: ", self.layer)

        def on_callback(type, image, speed, layer):
            self.type = type
            self.image = image
            self.speed = speed
            self.layer = layer

        dialog.set_callback(on_callback)


class UnimplementedObject(GameObj):

    def __init__(self, sexpr=[]):
        super().__init__()
        self.sexpr = sexpr

    def save(self, f):
        f.write("           (sexpr %s)\n" % [self.sexpr])
        # TODO


class LevelTime(GameObj):

    def __init__(self, sexpr=[]):
        self.time = get_value_from_tree(["time", "_"], sexpr, 999)

    def save(self, f, obj):
        f.write("       (leveltime\n")
        f.write("         (time %f)\n" % [self.time])
        f.write("       )\n")

    def property_dialog(self, gui):
        dialog = gui.gui.create_generic_dialog("LevelTime Property Dialog")
        dialog.add_float("Time: ", self.time)

        def on_callback(time):
            self.time = time

        dialog.set_callback(on_callback)


class Door(GameObj):

    def __init__(self, type, data, sexpr=[]):
        self.type = type
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

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("       (%s\n" % [self.type])
        f.write("         (x %d) (y %d)" % [pos.x, pos.y])
        f.write("         (sector \"%s\")\n" % self.sector)
        f.write("         (spawnpoint \"%s\")\n" % self.spawnpoint)
        f.write("       )\n")

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
        self.node = node

    def save(self, f, obj):
        pass


class ScriptedObject(GameObj):

    def __init__(self, object, sexpr=[]):
        self.object = object
        self.name = get_value_from_tree(["name", "_"], sexpr, "")
        self.sprite = get_value_from_tree(["sprite", "_"], sexpr, "")
        self.visible = get_value_from_tree(["visible", "_"], sexpr, True)
        self.physic_enabled = get_value_from_tree(["physic-enabled", "_"], sexpr, False)
        self.solid = get_value_from_tree(["solid", "_"], sexpr, False)
        self.layer = get_value_from_tree(["layer", "_"], sexpr, 100)
        self.load_sprite()

    def save(self, f, obj):
        pos = obj.get_pos()
        f.write("      (scriptedobject\n")
        f.write("        (x %d) (y %d)\n" % (pos.x, pos.y))
        f.write("        (name \"%s\")\n" % self.name)
        f.write("        (sprite \"%s\")\n" % self.sprite)
        f.write("        (layer %d)\n" % self.layer)

        if self.visible:
            f.write("        (visible #t)\n")
        else:
            f.write("        (visible #f)\n")

        if self.physic_enabled:
            f.write("        (physic-enabled #t)\n")
        else:
            f.write("        (physic-enabled #f)\n")

        if self.solid:
            f.write("        (solid #t)\n")
        else:
            f.write("        (solid #f)\n")

        f.write("      )\n")

    def load_sprite(self):
        sprite = Sprite.from_file(Config.current.datadir + self.sprite)
        self.object.set_sprite(sprite)

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
