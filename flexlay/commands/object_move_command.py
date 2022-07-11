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


class Obj:

    def __init__(self) -> None:
        self.obj = None
        self.old_pos = None
        self.new_pos = None


class ObjectMoveCommand:

    def __init__(self, object_layer: ObjectLayer) -> None:
        self.objmap = object_layer
        self.objects = []

    def execute(self) -> None:
        pass

    def add_obj(self, obj) -> None:
        o = Obj()
        o.obj = obj
        o.old_pos = obj.get_pos()
        self.objects.append(o)

    def move_by(self, delta) -> None:
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
