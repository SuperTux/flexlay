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


from typing import Any, List, Optional, TYPE_CHECKING

import os

from PyQt5.QtWidgets import QWidget

from flexlay.color import Colorf
from flexlay.gui.generic_dialog import GenericDialog
from flexlay.math import Rectf, Pointf, Sizef
from flexlay.objmap_object import ObjMapObject
from flexlay.objmap_rect_object import ObjMapRectObject
from flexlay.objmap_sprite_object import ObjMapSpriteObject
from flexlay.property import Property
from flexlay.util.config import Config
from flexlay.util.sexpr_writer import SExprWriter
from flexlay.workspace import Workspace

from supertux.constraint import Constraint
from supertux.sprite import SuperTuxSprite
from supertux.gameobj_props_change_command import GameObjPropsChangeCommand

if TYPE_CHECKING:
    from supertux.gui import SuperTuxGUI


def make_sprite_object(metadata: Any, filename: str, pos: Optional[Pointf] = None) -> ObjMapSpriteObject:
    pos = Pointf(0, 0) if pos is None else pos
    assert Config.current is not None
    st_sprite = SuperTuxSprite.from_file(os.path.join(Config.current.datadir, filename))
    sprite = st_sprite.get_sprite()
    assert sprite is not None
    obj = ObjMapSpriteObject(sprite, pos, metadata)
    return obj


def make_rect_object(metadata: Any, color: Optional[Colorf] = None) -> ObjMapRectObject:
    if color is None:
        color = Colorf(0, 0, 1, 0.5)
    pos = Pointf(0, 0)
    size = Sizef(64, 64)
    obj = ObjMapRectObject(Rectf.from_ps(pos, size), color.to_i(), metadata)
    return obj


class GameObj:

    label: str
    identifier: str
    sprite: str
    properties: list[Property] = []
    constraints: List[Constraint] = []

    def __init__(self) -> None:
        self.objmap_object: Optional[ObjMapObject] = None

    def signal_connect(self) -> None:
        """Connect the objmap_object signals to the on_select and on_deselect

        Used, for example, to display properties in PropertiesWidget.
        """
        if not self.objmap_object:
            return
        self.objmap_object.sig_select.connect(self.on_select)
        self.objmap_object.sig_deselect.connect(self.on_deselect)

    def on_select(self, manager: 'SuperTuxGUI') -> None:
        if manager:
            manager.properties_widget.set_properties(self.properties)
            manager.properties_widget.add_callback(self.on_callback)

    def on_deselect(self, manager: 'SuperTuxGUI') -> None:
        if manager:
            manager.properties_widget.clear_properties()
            manager.properties_widget.call_signal.clear()

    def add_property(self, prop: Property) -> None:
        self.properties.append(prop)

    def read(self, sexpr: Any) -> None:
        for prop in self.properties:
            prop.read(sexpr, self.objmap_object)

    def write(self, writer: SExprWriter, obj: ObjMapObject) -> None:
        writer.begin_list(self.identifier)
        for prop in self.properties:
            prop.write(writer, obj)
        writer.end_list()

    def property_dialog(self, parent: QWidget) -> None:
        assert self.label is not None
        dialog = GenericDialog(self.label + " Property Dialog", parent)
        for prop in self.properties:
            prop.property_dialog(dialog)

        dialog.add_callback(self.on_callback)

    def find_property(self, identifier: str) -> Optional[Property]:
        for prop in self.properties:
            if prop.identifier == identifier:
                return prop
        return None

    def on_callback(self, *args: Any) -> None:
        """Called when "Apply" or "Okay" hit"""
        assert Workspace.current is not None
        print(args)
        prop_diff = []
        i = 0
        for property in self.properties:
            if property.editable:
                if args[i] == property.value:
                    continue
                prop_diff.append((i, args[i], property.value))
            i += 1
        command = GameObjPropsChangeCommand(self, prop_diff)
        Workspace.current.get_map().execute(command)

    def update(self) -> None:
        """Called after properties read, and optionally at any other time"""
        pass


# EOF #
