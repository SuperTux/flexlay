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


from flexlay import (Color, ObjMapSpriteObject, ObjMapRectObject, ObjectAddCommand, Sprite)
from flexlay.math import Point, Pointf, Rect, Size
from flexlay.util import get_value_from_tree

from .config import Config
from .gameobj import (BadGuy, Dispenser, SpawnPoint, AmbientSound,
                      SimpleObject, SimpleTileObject, Powerup,
                      SecretArea, SequenceTrigger, Door, Background,
                      Gradient, ParticleSystem, Platform,
                      ScriptedObject, InfoBlock, LevelTime)


game_objects = [
    #["angrystone", "images/creatures/angrystone/angrystone.sprite", "sprite",
    # lambda data, sexpr: BadGuy("angrystone")],
    ["jumpy", "images/creatures/jumpy/left-middle.png", "sprite",
     lambda data, sexpr: BadGuy("jumpy")],
    ["snowball", "images/creatures/snowball/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("snowball")],
    ["mriceblock", "images/creatures/mr_iceblock/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("mriceblock")],
    ["mrbomb", "images/creatures/mr_bomb/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("mrbomb")],
    ["flame", "images/creatures/flame/flame-0.png", "sprite",
     lambda data, sexpr: BadGuy("flame")],
    ["stalactite", "images/creatures/stalactite/falling.png", "sprite",
     lambda data, sexpr: BadGuy("stalactite")],
    ["fish", "images/creatures/fish/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("fish")],
    ["flyingsnowball", "images/creatures/flying_snowball/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("flyingsnowball")],
    ["bouncingsnowball", "images/creatures/bouncing_snowball/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("bouncingsnowball")],
    ["spiky", "images/creatures/spiky/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("spiky")],
    ["mrtree", "images/creatures/mr_tree/walk-left-1.png", "sprite",
               lambda data, sexpr: BadGuy("mrtree")],
    ["poisonivy", "images/creatures/poison_ivy/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("poisonivy")],
    ["zeekling", "images/creatures/zeekling/left-0.png", "sprite",
     lambda data, sexpr: BadGuy("zeekling")],
    ["kugelblitz", "images/creatures/kugelblitz/flying-0.png", "sprite",
     lambda data, sexpr: BadGuy("kugelblitz")],
    ["dispenser", "images/creatures/dispenser/working.png", "sprite",
     lambda data, sexpr: Dispenser(data, sexpr)],
    ["yeti", "images/creatures/yeti/yeti.png", "sprite",
     lambda data, sexpr: BadGuy("yeti")],
    ["stalactite_yeti", "images/engine/editor/stalactite_yeti.png", "sprite",
     lambda data, sexpr: BadGuy("yeti_stalactite")],
    ["spawnpoint", "images/engine/editor/spawnpoint.png", "sprite",
     lambda data, sexpr: SpawnPoint(data, sexpr)],
    ["ambient_sound", "images/engine/editor/ambientsound.png", "rect",
     lambda data, sexpr: AmbientSound(data, sexpr)],
    ["door", "images/objects/door/door-0.png", "sprite",
     lambda data, sexpr: Door("door", data, sexpr)],
    #  ["hatch", "images/objects/hatch/hatch-0.png", "sprite",
    #    lambda data, sexpr: Door("hatch", data, sexpr)],
    ["trampoline", "images/objects/trampoline/trampoline1-0.png", "sprite",
     lambda data, sexpr: BadGuy("trampoline")],
    #  ["bell", "images/objects/bell/bell.sprite", "sprite",
    #    lambda data, sexpr: SimpleObject("bell")],
    ["rock", "images/tiles/blocks/block11.png", "sprite",
     lambda data, sexpr: SimpleObject("rock")],
    ["unstable_tile", "images/objects/unstable_tile/crumbling-1.png", "sprite",
     lambda data, sexpr: SimpleTileObject(data, "unstable_tile", sexpr)],
    ["infoblock", "images/engine/editor/infoblock.png", "sprite",
     lambda data, sexpr: InfoBlock(data, sexpr)],
    ["powerup", "images/engine/editor/powerup.png", "sprite",
     lambda data, sexpr: Powerup(data, sexpr)],
    ["secretarea", "images/engine/editor/secretarea.png", "rect",
     lambda data, sexpr: SecretArea(data, sexpr)],
    ["sequencetrigger", "images/engine/editor/sequencetrigger.png", "rect",
     lambda data, sexpr: SequenceTrigger(data, sexpr)],
    ["background", "images/engine/editor/background.png", "sprite",
     lambda data, sexpr: Background(data, sexpr)],
    ["gradient", "images/engine/editor/gradient.png", "sprite",
     lambda data, sexpr: Gradient(data, sexpr)],
    ["particles-snow", "images/engine/editor/snow.png", "sprite",
     lambda data, sexpr: ParticleSystem("snow", sexpr)],
    ["particles-clouds", "images/engine/editor/clouds.png", "sprite",
     lambda data, sexpr: ParticleSystem("clouds", sexpr)],
    ["particles-rain", "images/engine/editor/rain.png", "sprite",
     lambda data, sexpr: ParticleSystem("rain", sexpr)],
    ["leveltime", "images/engine/editor/clock.png", "sprite",
     lambda data, sexpr: LevelTime(sexpr)],
    ["point", "images/engine/editor/point.png", "sprite",
     lambda data, sexpr: SimpleObject("point")],
    ["platform", "images/objects/flying_platform/flying_platform-0.png", "sprite",
     lambda data, sexpr: Platform(data, sexpr)],
    ["scriptedobject", "images/engine/editor/scriptedobject.png", "sprite",
     lambda data, sexpr: ScriptedObject(data, sexpr)]
]


def find_game_object(name):
    result = [x for x in game_objects if x[0] == name]
    if result != []:
        return result[0]
    else:
        return None


def create_gameobject_from_data(editormap, objmap, name, sexpr):
    # Creates a gameobject from the given sexpr: "snowball", ((x 5) (y 5))
    obj = find_game_object(name)
    if obj is not None:
        x = get_value_from_tree(["x", "_"], sexpr, 0)
        y = get_value_from_tree(["y", "_"], sexpr, 0)

        create_gameobject(editormap, objmap, obj, Pointf(x, y), sexpr)
    else:
        print("Error: Couldn't resolve object type: ", name)
        print("Sector: Unhandled tag: ", name)


def create_gameobject(editormap, objmap, data, pos, sexpr):
    _, spritefile, kind, func = data

    # Creates a gameobject the given position, data is the entry in the game_objects table
    if kind == "sprite":
        sprite = Sprite.from_file(Config.current.datadir + spritefile)

        obj = ObjMapSpriteObject(sprite, pos, None)
        gobj = func.call(obj, sexpr)
        obj.metadata = gobj
        gobj.set_obj(obj)

    elif kind == "rect":
        print("NewRect", pos.x, " -", pos.y)
        obj = ObjMapRectObject(Rect(Point(pos.x, pos.y), Size(64, 64)),
                               Color(0, 0, 255, 128),
                               None)
        gobj = data[3].call(obj, sexpr)
        obj.metadata = gobj
    else:
        raise Exception("Error: Unknown object type dropped: %r" % data)

    cmd = ObjectAddCommand(objmap)
    cmd.add_object(obj)
    editormap.execute(cmd)
    return obj

solid_itiles = [10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 30, 31, 113, 114]

itile_conditions = [
    [0, 0, 0, 0, 0, 1, 0, 1, 1, 7],
    [0, 0, 1, 0, 0, 1, 0, 1, 1, 7],
    [0, 0, 0, 0, 0, 0, 0, 1, 1, 7],
    [0, 0, 0, 0, 0, 0, 1, 1, 1, 8],
    [0, 0, 0, 0, 0, 0, 1, 1, 0, 9],
    [0, 1, 1, 0, 0, 0, 0, 0, 0, 16],

    [1, 1, 1, 0, 0, 0, 0, 0, 0, 17],
    [1, 1, 1, 1, 0, 0, 0, 0, 0, 17],
    [1, 1, 1, 0, 0, 1, 0, 0, 0, 17],
    [1, 1, 1, 1, 0, 0, 1, 0, 0, 17],
    [1, 1, 1, 0, 0, 1, 0, 0, 1, 17],

    [1, 1, 0, 0, 0, 0, 0, 0, 0, 18],

    [0, 1, 1, 0, 1, 1, 0, 0, 0, 10],
    [1, 1, 1, 0, 1, 1, 0, 0, 0, 11],
    [1, 1, 0, 1, 1, 0, 0, 0, 0, 12],

    [0, 1, 1, 0, 1, 1, 0, 1, 1, 10],
    [1, 1, 1, 1, 1, 1, 1, 1, 1, 11],
    [1, 1, 0, 1, 1, 0, 1, 1, 0, 12],

    [0, 0, 0, 0, 1, 1, 0, 1, 1, 13],
    [0, 0, 0, 1, 1, 1, 1, 1, 1, 14],
    [0, 0, 0, 1, 1, 0, 1, 1, 0, 15],
    [1, 0, 0, 1, 1, 1, 1, 1, 1, 20],
    [1, 1, 0, 1, 1, 0, 1, 1, 1, 21],
    [0, 1, 1, 0, 1, 1, 1, 1, 1, 22],
    [0, 0, 1, 1, 1, 1, 1, 1, 1, 23],

    [1, 1, 1, 1, 1, 0, 1, 1, 0, 30],
    [1, 1, 1, 0, 1, 1, 0, 1, 1, 31],

    [0, 0, 0, 1, 1, 0, 1, 1, 1, 113],
    [0, 0, 0, 0, 1, 1, 1, 1, 1, 114],
]

# EOF #
