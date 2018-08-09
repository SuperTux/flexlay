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


from typing import List, Optional

import os

from flexlay import (Color, Config, ObjMapSpriteObject,
                     ObjMapRectObject, Workspace)
from flexlay.property import Property
from flexlay.gui import GenericDialog
from flexlay.math import Rect, Size, Point
from supertux.constraint import Constraint
from supertux.sprite import SuperTuxSprite
from supertux.gameobj_props_change_command import GameObjPropsChangeCommand


def make_sprite_object(metadata, filename, pos=None):
    pos = Point(0, 0) if pos is None else pos
    sprite = SuperTuxSprite.from_file(os.path.join(Config.current.datadir, filename))
    obj = ObjMapSpriteObject(sprite.get_sprite(), pos, metadata)
    return obj


def make_rect_object(metadata, color=None):
    if color is None:
        color = Color(0, 0, 255, 128)
    pos = Point(0, 0)
    size = Size(64, 64)
    obj = ObjMapRectObject(Rect(pos, size), color, metadata)
    return obj


class GameObj:

    label: Optional[str] = None
    identifier: Optional[str] = None
    properties: List[Property] = []
    constraints: List[Constraint] = []
    factory = None

    def __init__(self):
        self.objmap_object = None

    def signal_connect(self):
        """Connect the objmap_object signals to the on_select and on_deselect

        Used, for example, to display properties in PropertiesWidget.
        """
        if not self.objmap_object:
            return
        self.objmap_object.sig_select.connect(self.on_select)
        self.objmap_object.sig_deselect.connect(self.on_deselect)

    def on_select(self, manager):
        if manager:
            props_widget = manager.properties_widget
            props_widget.set_properties(self.properties)
            props_widget.add_callback(self.on_callback)

    def on_deselect(self, manager):
        if manager:
            props_widget = manager.properties_widget
            props_widget.clear_properties()
            props_widget.call_signal.clear()

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

    def property_dialog(self, gui=None):
        dialog = GenericDialog(self.label + " Property Dialog", gui)
        for prop in self.properties:
            prop.property_dialog(dialog)

        dialog.add_callback(self.on_callback)

    def find_property(self, identifier):
        for prop in self.properties:
            if prop.identifier == identifier:
                return prop

    def on_callback(self, *args):
        """Called when "Apply" or "Okay" hit"""
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

    def update(self):
        """Called after properties read, and optionally at any other time"""
        pass


# EOF #
