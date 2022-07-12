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
from flexlay.math import Pointf
from flexlay.commands import Command


class Obj:

    def __init__(self, objmap_object: ObjMapObject, old_pos: Pointf, new_pos: Pointf) -> None:
        self.obj: ObjMapObject = objmap_object
        self.old_pos: Pointf = old_pos
        self.new_pos: Pointf = new_pos


class ObjectMoveCommand(Command):

    def __init__(self, object_layer: ObjectLayer) -> None:
        self.objmap = object_layer
        self.objects: list[Obj] = []

    def execute(self) -> None:
        pass

    def add_obj(self, obj: ObjMapObject) -> None:
        self.objects.append(Obj(obj, obj.get_pos(), obj.get_pos()))

    def move_by(self, delta: Pointf) -> None:
        for obj in self.objects:
            obj.new_pos = obj.old_pos + delta
            obj.obj.set_pos(obj.new_pos)
            obj.obj.sig_move(obj.obj)

    def redo(self) -> None:
        for obj in self.objects:
            obj.obj.set_pos(obj.new_pos)

    def undo(self) -> None:
        for obj in self.objects:
            obj.obj.set_pos(obj.old_pos)


# EOF #
