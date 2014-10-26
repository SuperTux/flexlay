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


from collections import OrderedDict
import os.path

from flexlay import ObjectBrush, Config
from .sprite import SuperTuxSprite
from .gameobj import (BadGuy, BonusBlock, Candle, Camera, Dispenser,
                      Door, SpawnPoint, ResetPoint, AmbientSound,
                      SimpleObject, SimpleTileObject, Powerup,
                      SecretArea, SequenceTrigger, Background,
                      Gradient, ParticleSystem, Platform,
                      ScriptedObject, InfoBlock, LevelTime, Decal,
                      ScriptTrigger, Switch, Torch, WeakBlock,
                      WilloWisp, Wind, Trampoline)


class SuperTuxGameObjFactory:

    """
    identifier: section in the .stl file, used for load and save, most of the time, also for Drag&drop
    sprite: used in the object selection and/or in the ObjMapSpriteObject
    functor: creates the object

    ObjectBrush(sprite, metadata)

    See editor/supertux-editor/LevelObjects/Objects.cs
    """

    def __init__(self):
        self.objects = OrderedDict()
        self.init_factories()

    def create_gameobj_at(self, identifier, pos):
        data = self.objects.get(identifier)
        if data is None:
            print("couldn't create: %r at %s" % (identifier, pos))
        else:
            _, constructor = data
            obj = constructor()
            obj.objmap_object.pos = pos
            return obj

    def create_gameobj(self, identifier, sexpr):
        data = self.objects.get(identifier)
        if data is None:
            print("couldn't create: %r" % identifier)
        else:
            _, constructor = data
            obj = constructor()
            obj.read(sexpr)
            return obj

    def create_object_brushes(self):
        return [ObjectBrush(SuperTuxSprite.from_file(os.path.join(Config.current.datadir, sprite)).get_sprite(),
                            identifier)
                for identifier, (sprite, constructor) in self.objects.items()]

    def add_object(self, gameobj_class):
        self.objects[gameobj_class.identifier] = (gameobj_class.sprite, gameobj_class)

    def add_badguy(self, identifier, sprite):
        self.objects[identifier] = (sprite, lambda: BadGuy(identifier, sprite))

    def add_simple_object(self, identifier, sprite):
        self.objects[identifier] = (sprite, lambda: SimpleObject(identifier, sprite))

    def add_simple_tile(self, identifier, sprite):
        self.objects[identifier] = (sprite, lambda: SimpleTileObject(identifier, sprite))

    def add_particle_system(self, identifier, sprite, kind):
        self.objects[identifier] = (sprite, lambda: ParticleSystem(kind, sprite))

    def init_factories(self):
        self.add_simple_object("point", "images/engine/editor/point.png")
        self.add_simple_object("rock", "images/tiles/blocks/block11.png")
        self.add_simple_object("heavycoin", "images/objects/coin/heavy_coin.png")

        self.add_simple_tile("unstable_tile", "images/objects/unstable_tile/crumbling-1.png")

        self.add_object(AmbientSound)
        self.add_object(Background)
        self.add_object(Camera)
        self.add_object(Decal)
        self.add_object(Dispenser)
        self.add_object(SequenceTrigger)
        self.add_object(ScriptTrigger)

        self.add_object(BonusBlock)
        self.add_object(Candle)
        self.add_object(Door)
        self.add_object(Gradient)
        self.add_object(InfoBlock)
        self.add_object(LevelTime)
        self.add_object(Platform)
        self.add_object(Powerup)
        self.add_object(ResetPoint)
        self.add_object(ScriptedObject)
        self.add_object(SecretArea)
        self.add_object(SpawnPoint)
        self.add_object(Switch)
        self.add_object(Torch)
        self.add_object(Trampoline)
        self.add_object(WeakBlock)
        self.add_object(WilloWisp)
        self.add_object(Wind)

        self.add_badguy("pneumatic-platform", "images/engine/editor/pneumaticplatform.png")
        self.add_badguy("bicycle-platform", "images/engine/editor/bicycleplatform.png")
        self.add_badguy("flying-platform", "images/objects/flying_platform/flying_platform.sprite")
        self.add_badguy("hurting_platform", "images/objects/sawblade/sawblade.sprite")

        self.add_particle_system("particles-clouds", "images/engine/editor/clouds.png", "clouds")
        self.add_particle_system("particles-rain", "images/engine/editor/rain.png", "rain")
        self.add_particle_system("particles-snow", "images/engine/editor/snow.png", "snow")

        self.add_badguy("angrystone", "images/creatures/angrystone/angrystone.sprite")
        self.add_badguy("bouncingsnowball", "images/creatures/bouncing_snowball/left-0.png")
        self.add_badguy("captainsnowball", "images/creatures/snowball/cpt-left-0.png")
        self.add_badguy("climbable", "images/engine/editor/climbable.png")
        self.add_badguy("coin", "images/objects/coin/coin.sprite")
        self.add_badguy("crystallo", "images/creatures/crystallo/crystallo.sprite")
        self.add_badguy("darttrap", "images/creatures/darttrap/darttrap.sprite")
        self.add_badguy("fish", "images/creatures/fish/left-0.png")
        self.add_badguy("flame", "images/creatures/flame/flame-0.png")
        self.add_badguy("flyingsnowball", "images/creatures/flying_snowball/left-0.png")
        self.add_badguy("ghostflame", "images/creatures/flame/ghostflame.sprite")
        self.add_badguy("ghosttree", "images/creatures/flame/ghostflame.sprite")
        self.add_badguy("goldbomb", "images/creatures/gold_bomb/gold_bomb.sprite")
        self.add_badguy("haywire", "images/creatures/haywire/haywire.sprite")
        self.add_badguy("icecrusher", "images/creatures/icecrusher/icecrusher.sprite")
        self.add_badguy("iceflame", "images/creatures/flame/iceflame.sprite")
        self.add_badguy("igel", "images/creatures/igel/igel.sprite")
        self.add_badguy("invisible_wall", "images/engine/editor/invisible_wall.png")
        self.add_badguy("ispy", "images/objects/ispy/ispy.sprite")
        self.add_badguy("jumpy", "images/creatures/jumpy/left-middle.png")
        self.add_badguy("kugelblitz", "images/creatures/kugelblitz/flying-0.png")
        self.add_badguy("lantern", "images/objects/lantern/lantern.sprite")
        self.add_badguy("livefire", "images/creatures/livefire/livefire.sprite")
        self.add_badguy("livefire_asleep", "images/creatures/livefire/livefire.sprite")
        self.add_badguy("livefire_dormant", "images/creatures/livefire/livefire.sprite")
        self.add_badguy("magicblock", "images/objects/magicblock/magicblock.sprite")
        self.add_badguy("mole", "images/creatures/mole/mole.sprite")
        self.add_badguy("mrbomb", "images/creatures/mr_bomb/left-0.png")
        self.add_badguy("mriceblock", "images/creatures/mr_iceblock/left-0.png")
        self.add_badguy("mrtree", "images/creatures/mr_tree/walk-left-1.png")
        self.add_badguy("nolok_01", "images/creatures/nolok/nolok.sprite")
        self.add_badguy("owl", "images/creatures/owl/owl.sprite")
        self.add_badguy("particles-ghosts", "images/engine/editor/ghostparticles.png")
        self.add_badguy("poisonivy", "images/creatures/poison_ivy/left-0.png")
        self.add_badguy("pushbutton", "images/objects/pushbutton/pushbutton.sprite")
        self.add_badguy("rustytrampoline", "images/objects/rusty-trampoline/rusty-trampoline.sprite")
        self.add_badguy("short_fuse", "images/creatures/short_fuse/short_fuse.sprite")
        self.add_badguy("skullyhop", "images/creatures/skullyhop/skullyhop.sprite")
        self.add_badguy("smartball", "images/creatures/snowball/left-1.png")
        self.add_badguy("smartblock", "images/creatures/mr_iceblock/smart_block/smart_block.sprite")
        self.add_badguy("snail", "images/creatures/snail/snail.sprite")
        self.add_badguy("snowball", "images/creatures/snowball/sport-left-1.png")
        self.add_badguy("snowman", "images/creatures/snowman/snowman.sprite")
        self.add_badguy("spidermite", "images/creatures/spidermite/spidermite.sprite")
        self.add_badguy("spiky", "images/creatures/spiky/left-0.png")
        self.add_badguy("spotlight", "images/objects/spotlight/spotlight_center.sprite")
        self.add_badguy("sspiky", "images/creatures/spiky/sleepingspiky.sprite")
        self.add_badguy("stalactite", "images/creatures/stalactite/falling.png")
        self.add_badguy("stalactite_yeti", "images/engine/editor/stalactite_yeti.png")
        self.add_badguy("stumpy", "images/creatures/mr_tree/stumpy.sprite")
        self.add_badguy("thunderstorm", "images/engine/editor/thunderstorm.png")
        self.add_badguy("toad", "images/creatures/toad/toad.sprite")
        self.add_badguy("totem", "images/creatures/totem/totem.sprite")
        self.add_badguy("walkingleaf", "images/creatures/walkingleaf/walkingleaf.sprite")
        self.add_badguy("walkingtree", "images/creatures/walkingleaf/walkingleaf.sprite")
        self.add_badguy("yeti", "images/creatures/yeti/yeti.png")
        self.add_badguy("yeti_stalactite", "images/engine/editor/stalactite_yeti.png")
        self.add_badguy("zeekling", "images/creatures/zeekling/left-0.png")


supertux_gameobj_factory = SuperTuxGameObjFactory()


# EOF #
