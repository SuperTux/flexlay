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

import os
import logging

from flexlay import Sprite
from flexlay.util import get_value_from_tree, sexpr_read_from_file


class SuperTuxSpriteAction:

    def __init__(self) -> None:
        self.name: Optional[str] = None
        self.image: Optional[str] = None
        self.x_offset: int = 0
        self.y_offset: int = 0

    def parse(self, sexpr: Any) -> None:
        self.name = get_value_from_tree(["name", "_"], sexpr, "default")
        self.x_offset = get_value_from_tree(["x-offset", "_"], sexpr, 0)
        self.y_offset = get_value_from_tree(["y-offset", "_"], sexpr, 0)
        # we only parse the first image for now as we don't have support for
        # animation in flexlay anyway
        self.image = get_value_from_tree(["images", "_"], sexpr, 0)


class SuperTuxSprite:

    def __init__(self) -> None:
        self.actions: dict[str, SuperTuxSpriteAction] = {}
        self.actions_default: Optional[SuperTuxSpriteAction] = None
        self.basedir: str = ""

    @staticmethod
    def from_png_file(filename: str) -> 'SuperTuxSprite':
        sprite = SuperTuxSprite()
        action = SuperTuxSpriteAction()
        action.name = "default"
        action.image = filename
        sprite.actions["default"] = action
        return sprite

    @staticmethod
    def from_sprite_file(filename: str) -> 'SuperTuxSprite':
        sprite = SuperTuxSprite()

        tree: Any = sexpr_read_from_file(filename)[0]
        if tree is None:
            raise Exception("Error: Couldn't load: '%s'" % filename)

        sprite.basedir = os.path.dirname(filename)

        for i in tree[1:]:
            if i[0] == "action":
                action = SuperTuxSpriteAction()
                action.parse(i[1:])
                assert action.name is not None
                sprite.actions[action.name] = action
                if sprite.actions_default is None or action.name == "default":
                    sprite.actions_default = action
            else:
                logging.warning("Unknown symbol '%s' in sprite '%s'" % (i[0], filename))

        return sprite

    @staticmethod
    def from_file(filename: str) -> 'SuperTuxSprite':
        if filename[-4:] == ".png":
            return SuperTuxSprite.from_png_file(filename)
        elif filename[-7:] == ".sprite":
            return SuperTuxSprite.from_sprite_file(filename)
        else:
            raise Exception("Unsupported sprite format '%s'" % filename)

    def get_sprite(self, action_name: str = "default") -> Optional[Sprite]:
        action = self.actions.get(action_name, self.actions_default)
        if action is None:
            logging.info("Kaputt:" + str(self.actions))
            return None
        else:
            assert action.image is not None
            sprite = Sprite.from_file(os.path.join(self.basedir, action.image))
            # FIXME:
            # sprite.set_frame_offset(0, Point(action.x_offset, action.y_offset))
            return sprite


# EOF #
