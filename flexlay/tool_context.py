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


from .tile_brush import TileBrush
from .gui.tile_selection import TileSelection


class ToolContext:

    current = None

    def __init__(self):
        ToolContext.current = self
        self.tile_brush = TileBrush(1, 1)
        self.tile_brush.put(0, 0, 0)
        self.tile_brush.set_opaque()

        self.tile_selection = TileSelection()

        self.object_selection = []

        self.tilemap_layer = None


# EOF #
