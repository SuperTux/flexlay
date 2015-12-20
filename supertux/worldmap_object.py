# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2015 Karkus476 <karkus476@yahoo.com>
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

from .gameobj import GameObj, make_sprite_object
from .property import *


class WorldmapLevel(GameObj):
    label = "WorldmapLevel"
    identifier = "level"
    sprite = "images/worldmap/common/leveldot_green.png"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Level File:", "name", ""),
            SpriteProperty("Sprite", "sprite"),
            InlineTilePosProperty()
        ]


class SpecialTile(GameObj):
    label = "SpecialTile"
    identifier = "special-tile"
    sprite = "images/worldmap/common/leveldot.sprite"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Name", "name", "main"),
            InlineTilePosProperty()
        ]


class WorldmapSpawnpoint(GameObj):
    label = "WorldmapSpawnpoint"
    identifier = "worldmap-spawnpoint"
    sprite = "images/worldmap/common/tux.sprite"

    def __init__(self):
        super().__init__()

        self.objmap_object = make_sprite_object(self, self.sprite)
        self.signal_connect()

        self.properties = [
            StringProperty("Name", "name", "main"),
            InlineTilePosProperty()
        ]