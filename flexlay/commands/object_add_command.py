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


from flexlay.commands.command import Command
from flexlay.object_layer import ObjectLayer
from flexlay.objmap_object import ObjMapObject


class ObjectAddCommand(Command):

    def __init__(self, objmap: ObjectLayer) -> None:
        super().__init__()

        self.objmap: ObjectLayer = objmap
        self.objs: list[ObjMapObject] = []

    def add_object(self, obj: ObjMapObject) -> None:
        self.objs.append(obj)

    def execute(self) -> None:
        for obj in self.objs:
            self.objmap.add_object(obj)

    def undo(self) -> None:
        for obj in self.objs:
            self.objmap.delete_object(obj)

    def redo(self) -> None:
        self.execute()


# EOF #
