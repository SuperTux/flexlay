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


import os

from flexlay import (Color, Config, ObjMapSpriteObject, ObjMapRectObject)
from flexlay.math import Point, Rect, Size

from .sprite import SuperTuxSprite
from .property import (BoolProperty, IntProperty, FloatProperty,
                       StringProperty, DirectionProperty,
                       InlinePosProperty, InlineRectProperty,
                       TilemapProperty, SpriteProperty,
                       SampleProperty, PathProperty, ColorProperty,
                       ImageProperty)


def make_sprite_object(metadata, filename):
    pos = Point(0, 0)
    sprite = SuperTuxSprite.from_file(os.path.join(Config.current.datadir, filename))
    obj = ObjMapSpriteObject(sprite.get_sprite(), pos, metadata)
    return obj


def make_rect_object(metadata, color=None):
    if color is None:
        color = Color(0, 0, 255, 128)
    obj = ObjMapRectObject(Rect(Point(0, 0), Size(64, 64)),
                           color, metadata)
    return obj


class GridConstrain:

    def __init__(self, width, height, offset_x, offset_y):
        pass


class GameObj:

    label = None
    identifier = None
    properties = []
    constraints = []

    def __init__(self):
        pass

    def add_property(self, prop):
        self.properties.append(prop)

    def read(self, sexpr):
        for prop in self.properties:
            prop.read(sexpr, self.objmap_object)

    def write(self, writer, obj):
        writer.begin_list(self.identifier)
        for prop in self.properties:
            prop.write(writer, obj)
        writer.end_list()

    def property_dialog(self, gui):
        dialog = gui.create_generic_dialog("SecretArea Property Dialog")
        for prop in self.properties:
            prop.property_dialog(dialog)

        def on_callback(*args):
            print(*args)

        dialog.set_callback(on_callback)


class ResetPoint(GameObj):

    label = "ResetPoint"
    identifier = "firefly"
    sprite = "images/objects/resetpoints/default-resetpoint.sprite"

    # ["bell", "images/objects/bell/bell.sprite", "sprite",
    #  lambda data, sexpr: SimpleObject("bell")],

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            InlinePosProperty()
        ]


class Decal(GameObj):

    label = "Decal"
    identifier = "decal"
    sprite = "images/engine/editor/resetpoint.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            InlinePosProperty(),
            IntProperty("Layer", "layer"),
            SpriteProperty("Sprite", "sprite")
        ]


class SecretArea(GameObj):

    label = "SecretArea"
    identifier = "secretarea"
    sprite = "images/engine/editor/secretarea.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_rect_object(self, Color(0, 255, 0))

        self.properties = [
            TilemapProperty("FadeTilemap", "fade-tilemap"),
            InlineRectProperty(),
            StringProperty("Message", "message", ""),
            SpriteProperty("Sprite", "sprite")
        ]


class AmbientSound(GameObj):

    label = "AmbientSound"
    identifier = "ambient_sound"
    sprite = "images/engine/editor/ambientsound.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_rect_object(self, Color(255, 0, 0))

        self.properties = [
            FloatProperty("Distance Factor", "distance_factor", 0.1),
            FloatProperty("Distance Bias", "distance_bias", 200),
            SampleProperty("Sample", "sample", "waterfall"),
            IntProperty("Max Volume", "volume", 1),
            InlineRectProperty()
        ]


class ScriptTrigger(GameObj):

    label = "ScriptTrigger"
    identifier = "scripttrigger"
    sprite = "images/engine/editor/scripttrigger.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_rect_object(self, Color(255, 0, 255))

        self.properties = [
            StringProperty("Script", "script", ""),
            BoolProperty("Button", "button", False),
            InlineRectProperty(),
        ]


class SequenceTrigger(GameObj):

    label = "SequenceTrigger"
    identifier = "sequencetrigger"
    sprite = "images/engine/editor/sequencetrigger.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_rect_object(self, Color(255, 0, 0))

        self.properties = [
            InlineRectProperty(),
            StringProperty("Sequence", "sequence", "")
        ]


class BadGuy(GameObj):

    def __init__(self, kind, sprite_filename):
        super().__init__()

        self.label = kind
        self.identifier = kind
        self.sprite = sprite_filename
        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            DirectionProperty("Direction", "direction", "left"),
            InlinePosProperty(),
        ]


class Dispenser(GameObj):

    label = "Dispenser"
    identifier = "dispenser"
    sprite = "images/creatures/dispenser/working.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            InlinePosProperty(),
            StringProperty("Badguy", "badguy", "snowball"),
            IntProperty("Cycle", "cycle", 2)
        ]


class Platform(GameObj):

    label = "Platform"
    identifier = "platform"
    sprite = "images/objects/flying_platform/flying_platform-0.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            InlinePosProperty(),
            StringProperty("Badguy", "badguy", "snowball"),
            PathProperty("Path", "use_path", None),
            StringProperty("Type", "type", None)
        ]


class SpawnPoint(GameObj):

    label = "SpawnPoint"
    identifier = "spawnpoint"
    sprite = "images/engine/editor/spawnpoint.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            StringProperty("Name", "name", None),
            InlinePosProperty()
        ]


class SimpleObject(GameObj):

    def __init__(self, kind):
        super().__init__()

        self.label = kind
        self.identifier = kind
        self.properties = [
            InlinePosProperty()
        ]


class SimpleTileObject(GameObj):

    def __init__(self, kind):
        super().__init__()

        self.label = kind
        self.identifier = kind
        self.properties = [
            InlinePosProperty()
        ]
        self.constraints = [
            GridConstrain(32, 32, 16, 16)
        ]


class InfoBlock(GameObj):

    label = "InfoBlock"
    identifier = "infoblock"
    sprite = "images/objects/bonus_block/infoblock.sprite"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            StringProperty("Message", "message"),
            InlinePosProperty()
        ]
        self.constraints = [
            GridConstrain(32, 32, 16, 16)
        ]


class Powerup(GameObj):

    label = "Powerup"
    identifier = "powerup"
    sprite = "images/engine/editor/powerup.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            InlinePosProperty(),
            SpriteProperty("Sprite", "sprite")
        ]
        self.constraints = [
            GridConstrain(32, 32, 16, 16)
        ]


class ParticleSystem(GameObj):

    def __init__(self, kind):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.label = "ParticleSystem (%s)" % kind
        self.identifier = "particles-%s" % kind
        self.properties = [
            IntProperty("Layer", "layer")
        ]


class Gradient(GameObj):

    label = "Gradient"
    identifier = "gradient"
    sprite = "images/engine/editor/gradient.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            ColorProperty("Top Color", "top_color"),
            ColorProperty("Bottom Color", "bottom_color"),
            IntProperty("Layer", "layer")
        ]


class Background(GameObj):

    label = "Background"
    identifier = "background"
    sprite = "images/engine/editor/background.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.properties = [
            FloatProperty("Speed (X)", "speed", optional=True),
            FloatProperty("Speed (Y)", "speed_y", optional=True),
            ImageProperty("Image (top)", "image-top"),
            ImageProperty("Image (middle)", "image"),
            ImageProperty("Image (bottom)", "image-bottom"),
            IntProperty("Layer", "layer", optional=True)
        ]


class UnimplementedObject(GameObj):

    def __init__(self):
        super().__init__()
        # self.sexpr = sexpr

    def save(self, f):
        f.write("           (sexpr %s)\n" % self.sexpr)
        # TODO


class LevelTime(GameObj):

    label = "LevelTime"
    identifier = "leveltime"
    sprite = "images/engine/editor/clock.png"

    def __init__(self):
        super().__init__()

        self.properties = [
            IntProperty("Time", "time", None)
        ]


class Door(GameObj):

    sprite = "images/objects/door/door-0.png"

    def __init__(self, kind):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)

        self.label = "Door (%s)" % kind
        self.identifier = kind
        self.properties = [
            InlinePosProperty(),
            StringProperty("Sector", "sector"),
            StringProperty("SpawnPoint", "spawnpoint"),
        ]
        self.constraints = [
            GridConstrain(32, 32, 16, 16)
        ]


class PathNode(GameObj):

    def __init__(self, node):
        super().__init__()
        self.node = node


class ScriptedObject(GameObj):

    label = "Scripted Object"
    identifier = "scriptedobject"
    sprite = "images/engine/editor/scriptedobject.png"

    def __init__(self):
        super().__init__()

        self.properties = [
            InlinePosProperty(),
            StringProperty("Name", "name"),
            SpriteProperty("Sprite", "sprite"),
            IntProperty("Layer", "layer"),
            BoolProperty("Visible", "visible"),
            BoolProperty("Physics", "physic-enabled"),
            BoolProperty("Solid", "solid"),
        ]


# EOF #
