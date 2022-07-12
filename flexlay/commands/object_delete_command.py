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


from flexlay.object_layer import ObjectLayer
from flexlay.objmap_object import ObjMapObject
from flexlay.commands import Command


class ObjectDeleteCommand(Command):

    def __init__(self, object_layer: ObjectLayer) -> None:
        self.object_layer = object_layer
        self.objects: list[ObjMapObject] = []

    def add_object(self, obj: ObjMapObject) -> None:
        self.objects.append(obj)

    def execute(self) -> None:
        for obj in self.objects:
            self.object_layer.delete_object(obj)

    def redo(self) -> None:
        self.execute()

    def undo(self) -> None:
        for obj in self.objects:
            self.object_layer.add_object(obj)


# EOF #
