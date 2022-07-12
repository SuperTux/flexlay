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


from typing import Optional

from flexlay.graphic_context import GraphicContext
from flexlay.layer import Layer
from flexlay.math import Pointf, Rectf
from flexlay.objmap_control_point import ObjMapControlPoint
from flexlay.objmap_object import ObjMapObject


class ObjectLayer(Layer):

    def __init__(self) -> None:
        super().__init__()

        self.objects: list[ObjMapObject] = []
        self.control_points: list[ObjMapControlPoint] = []

    def has_bounding_rect(self) -> bool:
        return False

    def draw(self, gc: GraphicContext) -> None:
        for obj in self.objects:
            rect = obj.get_bound_rect()
            if obj.to_draw and (rect is None or gc.get_clip_rect().is_overlapped(rect)):
                obj.draw(gc)

        for cp in self.control_points:
            cp.draw(gc)

    def find_control_point(self, click_pos: Pointf) -> Optional[ObjMapControlPoint]:
        for cp in reversed(self.control_points):
            rect = cp.get_bound_rect()
            if rect is not None and rect.is_inside(click_pos):
                return cp
        return None

    def find_object(self, click_pos: Pointf) -> Optional[ObjMapObject]:
        for obj in reversed(self.objects):
            if obj.is_inside(click_pos):
                return obj
        return None

    def delete_object(self, obj: ObjMapObject) -> None:
        self.objects.remove(obj)

    def delete_objects(self, objs: list[ObjMapObject]) -> None:
        for obj in objs:
            self.objects.remove(obj)

    def get_selection(self, rect: Rectf) -> list[ObjMapObject]:
        selection = []
        for obj in self.objects:
            if rect.is_inside(obj.get_pos()):
                selection.append(obj)

        return selection

    def get_objects(self) -> list[ObjMapObject]:
        return self.objects

    def add_object(self, obj: ObjMapObject) -> None:
        self.objects.append(obj)

    def add_control_point(self, obj: ObjMapControlPoint) -> None:
        self.control_points.append(obj)

    def delete_control_points(self) -> None:
        self.control_points.clear()

    def get_object_index(self, needle: ObjMapObject) -> int:
        for idx, obj in enumerate(self.objects):
            if obj == needle:
                return idx
        return -1

    def raise_objects_to_top(self, objs: list[ObjMapObject]) -> None:
        self.delete_objects(objs)
        self.objects.extend(objs)

    def lower_objects_to_bottom(self, objs: list[ObjMapObject]) -> None:
        self.delete_objects(objs)
        self.objects = objs + self.objects

    def raise_object(self, obj: ObjMapObject) -> None:
        i = self.get_object_index(obj)
        if i != -1 and len(self.objects) > 1 and i < len(self.objects) - 1:
            self.objects[i], self.objects[i + 1] = self.objects[i + 1], self.objects[i]

    def lower_object(self, obj: ObjMapObject) -> None:
        i = self.get_object_index(obj)
        if i != -1 and i > 0:
            self.objects[i], self.objects[i - 1] = self.objects[i - 1], self.objects[i]


# EOF #
