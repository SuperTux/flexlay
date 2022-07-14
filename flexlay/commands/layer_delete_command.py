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


from typing import Optional, TYPE_CHECKING

from flexlay.commands.command import Command
from flexlay.objmap_tilemap_object import ObjMapTilemapObject

if TYPE_CHECKING:
    from flexlay.gui.layer_selector import LayerSelector


class LayerDeleteCommand(Command):
    def __init__(self, layer_selector: 'LayerSelector', layer: int) -> None:
        """Deletes a Layer

        :param layer: Either a TilemapLayer, an ObjMapTilemapObject or an int (the layer to remove)
        """
        self.layer: int = layer
        self.layer_selector: 'LayerSelector' = layer_selector
        self.removed_object: Optional[ObjMapTilemapObject] = None

    def execute(self) -> None:
        self.removed_object = self.layer_selector.unsafe_remove_layer(self.layer)

    def undo(self) -> None:
        if self.removed_object is not None:
            self.layer_selector.add_layer(self.removed_object)
        else:
            raise RuntimeError("Could not undo layer removal.")

    def redo(self) -> None:
        assert self.removed_object is not None
        self.layer_selector.unsafe_remove_layer(self.removed_object)


# EOF #
