# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from typing import Any

from flexlay.sprite import Sprite


class ObjectBrush:

    def __init__(self, sprite: Sprite, metadata: Any, variable: bool = False) -> None:
        self.sprite: Sprite = sprite
        self.metadata: Any = metadata
        self.variable: bool = variable

    def check_sprite(self) -> None:
        pass

    def get_sprite(self) -> Sprite:
        return self.sprite

    def get_data(self) -> Any:
        return self.metadata


# EOF #
