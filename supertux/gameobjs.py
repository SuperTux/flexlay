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


from typing import Any, Optional

import logging
from enum import IntEnum

from PyQt5.QtWidgets import QWidget

from flexlay.objmap_path_node import ObjMapPathNode
from flexlay.util.sexpr_writer import SExprWriter
from flexlay import (Colorf, ObjMapObject)
from flexlay.math import Pointf
from flexlay.property import (
    BoolProperty,
    ColorProperty,
    EnumProperty,
    FloatProperty,
    IntProperty,
    StringProperty,
)
from supertux.property import (
    BadGuyProperty,
    DirectionProperty,
    ImageProperty,
    InlinePosProperty,
    InlineRectProperty,
    PathProperty,
    SampleProperty,
    SectorProperty,
    SpriteProperty,
    TilemapProperty,
    ZPosProperty,
)
from supertux.gameobj import GameObj, make_sprite_object, make_rect_object
from supertux.constraint import GridConstraint


class Layer(IntEnum):

    BACKGROUND0 = -300
    BACKGROUND1 = -200
    BACKGROUNDTILES = -100
    TILES = 0
    OBJECTS = 50
    FLOATINGOBJECTS = 150
    FOREGROUNDTILES = 200
    FOREGROUND0 = 300
    FOREGROUND1 = 400
    LIGHTMAP = 450
    HUD = 500,
    GUI = 600


class Tux(GameObj):
    """Unfinished:

    To be dragged to where you want tux to spawn when you run the level, but *is not*
    the spawnpoint.
    """

    label = "Tux"
    identifier = "tux"
    sprite = "images/creatures/tux/big/jump-0.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

    def read(self, sexpr: Any) -> None:
        pass

    def write(self, writer: SExprWriter, obj: Any) -> None:
        pass


class LevelTime(GameObj):

    label = "LevelTime"
    identifier = "leveltime"
    sprite = "images/engine/editor/clock.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            IntProperty("Time", "time", None)
        ]


class Camera(GameObj):

    label = "Camera"
    identifier = "camera"
    sprite = None
    values = ["normal", "autoscroll"]

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = ObjMapObject(Pointf(0, 0), self)
        self.objmap_object.to_draw = False
        self.signal_connect()

        self.properties = [
            EnumProperty("Mode", "mode", default=0, optional=False, values=self.values),
            BoolProperty("Backscrollling", "backscrolling", optional=True, default=True),
            PathProperty("Path", "path")
        ]


class ResetPoint(GameObj):

    label = "ResetPoint"
    identifier = "firefly"
    sprite = "images/objects/resetpoints/default-resetpoint.sprite"

    # ["bell", "images/objects/bell/bell.sprite", "sprite",
    #  lambda data, sexpr: SimpleObject("bell")],

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            SpriteProperty("Sprite", "sprite"),
            InlinePosProperty()
        ]


class GhostFlame(GameObj):

    label = "GhostFlame"
    identifier = "ghostflame"
    sprite = "images/creatures/flame/ghostflame.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            FloatProperty("Radius", "radius", default=100.0, optional=True),
            FloatProperty("Speed", "speed", default=2.0, optional=True),
            InlinePosProperty(),
        ]


class Flame(GameObj):

    label = "Flame"
    identifier = "flame"
    sprite = "images/creatures/flame/flame.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            FloatProperty("Radius", "radius", default=100.0, optional=True),
            FloatProperty("Speed", "speed", default=2.0, optional=True),
            InlinePosProperty(),
        ]


class Decal(GameObj):

    label = "Decal"
    identifier = "decal"
    sprite = "images/engine/editor/decal.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            ZPosProperty(),
            SpriteProperty("Sprite", "sprite"),
            InlinePosProperty(),
        ]

    def update(self) -> None:
        prop = self.find_property("sprite")
        assert prop is not None
        self.sprite = prop.value
        self.objmap_object = make_sprite_object(self, self.sprite,
                                                Pointf(self.objmap_object.pos.x, self.objmap_object.pos.y))


class Trampoline(GameObj):

    label = "Trampoline"
    identifier = "trampoline"
    sprite = "images/objects/trampoline/trampoline.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            BoolProperty("Portable", "portable", optional=True, default=True),
            InlinePosProperty()
        ]


class Coin(GameObj):

    label = "Coin"
    identifier = "coin"
    sprite = "images/objects/coin/coin.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            PathProperty("Path", "path")
        ]


class Climbable(GameObj):

    label = "Climbable"
    identifier = "climbable"
    sprite = "images/engine/editor/climbable.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_rect_object(self, Colorf(1.0, 1.0, 0))
        self.signal_connect()

        self.properties = [
            InlineRectProperty(),
        ]


class SecretArea(GameObj):

    label = "SecretArea"
    identifier = "secretarea"
    sprite = "images/engine/editor/secretarea.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_rect_object(self, Colorf(0, 1.0, 0))
        self.signal_connect()

        self.properties = [
            TilemapProperty("FadeTilemap", "fade-tilemap", optional=True, placeholder="No tilemap"),
            StringProperty("Script", "script", "", optional=True, placeholder="Empty script"),
            StringProperty("Message", "message", "", optional=True, translatable=False, placeholder="Empty message"),
            InlineRectProperty(),
        ]


class InvisibleWall(GameObj):

    label = "InvisibleWall"
    identifier = "invisible_wall"
    sprite = "images/engine/editor/invisible_wall.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_rect_object(self, Colorf(0, 0, 0, 0.8))
        self.signal_connect()

        self.properties = [
            InlineRectProperty(),
        ]


class AmbientSound(GameObj):

    label = "AmbientSound"
    identifier = "ambient_sound"
    sprite = "images/engine/editor/ambientsound.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_rect_object(self, Colorf(1.0, 0, 0))
        self.signal_connect()

        self.properties = [
            FloatProperty("Distance Factor", "distance_factor", 0.1),
            FloatProperty("Distance Bias", "distance_bias", 200),
            SampleProperty("Sample", "sample", ""),
            IntProperty("Max Volume", "volume", 1),
            InlineRectProperty()
        ]


class ScriptTrigger(GameObj):

    label = "ScriptTrigger"
    identifier = "scripttrigger"
    sprite = "images/engine/editor/scripttrigger.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_rect_object(self, Colorf(1.0, 0, 1.0))
        self.signal_connect()

        self.properties = [
            StringProperty("Script", "script", ""),
            BoolProperty("Button", "button", False),
            InlineRectProperty(),
        ]


class SequenceTrigger(GameObj):

    label = "SequenceTrigger"
    identifier = "sequencetrigger"
    sprite = "images/engine/editor/sequencetrigger.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_rect_object(self, Colorf(1.0, 0, 0))
        self.signal_connect()

        self.properties = [
            StringProperty("Sequence", "sequence", ""),
            InlineRectProperty(),
        ]


class BadGuy(GameObj):

    def __init__(self, kind: str, sprite_filename: str) -> None:
        super().__init__()

        self.label = kind
        self.identifier = kind
        self.sprite = sprite_filename
        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Dead Script", "dead-script", default="", optional=True),
            DirectionProperty("Direction", "direction", 0),
            SpriteProperty("Sprite", "sprite", default="", optional=True),
            InlinePosProperty(),
        ]


class Candle(GameObj):

    label = "Candle"
    identifier = "candle"
    sprite = "images/objects/candle/candle.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Name", "name", "", optional=True),
            BoolProperty("Burning", "burning", optional=True, default=True),
            BoolProperty("Flicker", "flicker", optional=True, default=True),
            SpriteProperty("Sprite", "sprite", default=self.sprite, optional=True),
            ColorProperty("Color", "color", default=Colorf(1.0, 1.0, 1.0), optional=True),
            InlinePosProperty(),
        ]


class Torch(GameObj):

    label = "Torch"
    identifier = "torch"
    sprite = "images/objects/torch/torch1.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            SpriteProperty("Sprite", "sprite", default=self.sprite, optional=True),
            InlinePosProperty(),
        ]


class DartTrap(GameObj):

    label = "DartTrap"
    identifier = "darttrap"
    sprite = "images/creatures/darttrap/darttrap.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            FloatProperty("Initial Delay", "initial-delay", default=0, optional=False),
            FloatProperty("Fire Delay", "fire-delay", default=2.0, optional=False),
            IntProperty("Ammo", "ammo", default=-1, optional=False),
            DirectionProperty("Direction", "direction", 0),
            InlinePosProperty(),
        ]


class WilloWisp(GameObj):

    label = "WillowIsp"
    identifier = "willowisp"
    sprite = "images/creatures/willowisp/willowisp.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            SectorProperty("Sector", "sector", default="", optional=False),
            StringProperty("SpawnPoint", "spawnpoint", default="", optional=False),
            FloatProperty("Fly Speed", "flyspeed", default=64.0, optional=True),
            FloatProperty("Track Range", "track-range", default=384.0, optional=True),
            FloatProperty("Vanish Range", "vanish-range", default=512, optional=True),
            StringProperty("Hit Script", "hit-script", default="", optional=True),
            # ColorProperty("Color", "color"),
            InlinePosProperty(),
        ]


class Dispenser(GameObj):

    label = "Dispenser"
    identifier = "dispenser"
    sprite = "images/creatures/dispenser/canon.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            IntProperty("Cycle", "cycle", 2),
            EnumProperty("Type", "type", default=0, optional=False, values=["rocketlauncher", "cannon"]),
            BoolProperty("Random", "random", default=False, optional=True),
            BadGuyProperty("Badguy", "badguy", GameObj.factory),
            DirectionProperty("Direction", "direction", 0),
            InlinePosProperty(),
        ]


class Switch(GameObj):

    label = "Switch"
    identifier = "switch"
    sprite = "images/objects/switch/switch.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Script", "script", default=""),
            SpriteProperty("Sprite", "sprite", default="", optional=True),
            DirectionProperty("Direction", "direction", 0),
            InlinePosProperty(),
        ]


class Platform(GameObj):
    label = "Platform"
    identifier = "platform"
    sprite = "images/objects/flying_platform/flying_platform-0.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Name", "name", "", optional=True),
            BoolProperty("Running", "running", default=True, optional=True),
            SpriteProperty("Sprite", "sprite"),
            PathProperty("Path", "path")
        ]


class SpawnPoint(GameObj):
    label = "SpawnPoint"
    identifier = "spawnpoint"
    sprite = "images/engine/editor/spawnpoint.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Name", "name", "main"),
            InlinePosProperty()
        ]


class SimpleObject(GameObj):

    def __init__(self, kind: str, sprite: str) -> None:
        super().__init__()

        self.label = kind
        self.identifier = kind
        self.sprite = sprite

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            InlinePosProperty()
        ]


class SimpleTileObject(GameObj):

    def __init__(self, kind: str, sprite: str) -> None:
        super().__init__()

        self.label = kind
        self.identifier = kind
        self.sprite = sprite

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            SpriteProperty("Sprite", "sprite"),
            InlinePosProperty()
        ]
        self.constraints = [
            GridConstraint(32, 32, 16, 16)
        ]


class WeakBlock(GameObj):

    label = "WeakBlock"
    identifier = "weak_block"
    sprite = "images/objects/weak_block/meltbox.sprite"
    sprite_linked = "images/objects/weak_block/strawbox.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            BoolProperty("Linked", "linked", default=False),
            InlinePosProperty()
        ]


class BonusBlock(GameObj):

    label = "BonusBlock"
    identifier = "bonusblock"
    sprite = "images/objects/bonus_block/bonusblock.sprite"
    values = [
        "coin",
        "1up",
        "custom",
        "explode",
        "firegrow",
        "icegrow",
        "light",
        "rain",
        "script",
        "star",
        "trampoline",
    ]

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Message", "message", default="", optional=True, translatable=True),
            StringProperty("Script", "script", default="", optional=False),
            IntProperty("Count", "count", default=1, optional=True),
            EnumProperty("Contents", "contents", default=0, optional=True, values=self.values),
            SpriteProperty("Sprite", "sprite", optional=True),
            InlinePosProperty()
        ]
        self.constraints = [
            GridConstraint(32, 32, 16, 16)
        ]


class InfoBlock(GameObj):

    label = "InfoBlock"
    identifier = "infoblock"
    sprite = "images/objects/bonus_block/infoblock.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Message", "message", "", optional=True, translatable=True),
            InlinePosProperty()
        ]
        self.constraints = [
            GridConstraint(32, 32, 16, 16)
        ]


class Powerup(GameObj):

    label = "Powerup"
    identifier = "powerup"
    sprite = "images/engine/editor/powerup.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            BoolProperty("Disable Physics", "disable-physics", default=False, optional=True),
            StringProperty("Script", "script", default="", optional=True),
            SpriteProperty("Sprite", "sprite", default=self.sprite, optional=False),
            InlinePosProperty(),
        ]
        self.constraints = [
            GridConstraint(32, 32, 16, 16)
        ]


class ParticleSystem(GameObj):

    def __init__(self, kind: str, sprite: str) -> None:
        super().__init__()

        self.label = "ParticleSystem (%s)" % kind
        self.identifier = "particles-%s" % kind
        self.sprite = sprite

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            ZPosProperty(),
        ]


class Gradient(GameObj):

    label = "Gradient"
    identifier = "gradient"
    sprite = "images/engine/editor/gradient.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            ZPosProperty(default=Layer.BACKGROUND0.value),
            ColorProperty("Top Color", "top_color"),
            ColorProperty("Bottom Color", "bottom_color"),
        ]


class Background(GameObj):

    label = "Background"
    identifier = "background"
    sprite = "images/engine/editor/background.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            FloatProperty("X", "x", default=0, optional=True),
            FloatProperty("Y", "y", default=0, optional=True),
            EnumProperty("Alignment", "alignment", default=0, optional=True,
                         values=["none", "left", "right", "top", "bottom"]),
            FloatProperty("Speed (X)", "speed", optional=False),
            FloatProperty("Speed (Y)", "speed_y", optional=True),
            ImageProperty("Image (top)", "image-top", optional=True),
            ImageProperty("Image (middle)", "image"),
            ImageProperty("Image (bottom)", "image-bottom", optional=True),
            ZPosProperty(default=Layer.BACKGROUND0),
        ]


class UnimplementedObject(GameObj):

    def __init__(self) -> None:
        super().__init__()
        self.sexpr = None

    def read(self, sexpr: Any) -> None:
        self.sexpr = sexpr

    def write(self, writer: SExprWriter, obj: Any) -> None:
        logging.debug("Unimplemented: " + str(self.sexpr))

    def property_dialog(self, dialog: Optional[QWidget]) -> None:
        pass


class Door(GameObj):

    label = "Door"
    identifier = "door"
    sprite = "images/objects/door/door.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Sector", "sector"),
            StringProperty("SpawnPoint", "spawnpoint"),
            InlinePosProperty(),
        ]
        self.constraints = [
            GridConstraint(32, 32, 16, 16)
        ]


class PathNode(GameObj):

    def __init__(self, node: ObjMapPathNode) -> None:
        super().__init__()
        self.node = node


class ScriptedObject(GameObj):

    label = "Scripted Object"
    identifier = "scriptedobject"
    sprite = "images/engine/editor/scriptedobject.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Name", "name"),
            ZPosProperty(),
            BoolProperty("Visible", "visible", True),
            BoolProperty("Physics", "physic-enabled", False),
            BoolProperty("Solid", "solid", False),
            SpriteProperty("Sprite", "sprite"),
            InlinePosProperty(),
        ]


class Wind(GameObj):

    label = "Wind"
    identifier = "wind"
    sprite = "images/engine/editor/wind.png"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_rect_object(self, Colorf(0, 0.75, 0.75, 0.75))
        self.signal_connect()

        self.properties = [
            StringProperty("Name", "name", default="", optional=True),
            BoolProperty("Blowing", "blowing", default=True, optional=True),
            FloatProperty("Speed-X", "speed-x", default=0.0, optional=False),
            FloatProperty("Speed-Y", "speed-y", default=0.0, optional=False),
            FloatProperty("Acceleration", "acceleration", default=0.0, optional=False),
            InlineRectProperty(),
        ]


class Pushbutton(GameObj):

    label = "Pushbutton"
    identifier = "pushbutton"
    sprite = "images/objects/pushbutton/pushbutton.sprite"

    def __init__(self) -> None:
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Script", "script", default=""),
            SpriteProperty("Sprite", "sprite", default="", optional=True),
            DirectionProperty("Direction", "direction", 0),
            InlinePosProperty(),
        ]


# EOF #
