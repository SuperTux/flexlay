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


from flexlay import Layer


class ObjectLayer(Layer):

    current = None

    def __init__(self):
        super().__init__()

        ObjectLayer.current = self

        self.objects = []
        self.control_points = []

    def has_bounding_rect(self):
        return False

    def draw(self, gc):
        for obj in self.objects:
            if gc.get_clip_rect().is_overlapped(obj.get_bound_rect()):
                obj.draw(gc)

        for cp in self.control_points:
            cp.draw(gc)

    def find_control_point(self, click_pos):
        for cp in reversed(self.control_points):
            rect = cp.get_bound_rect()
            if rect.is_inside(click_pos):
                return cp
        return None

    def find_object(self, click_pos):
        for obj in reversed(self.objects):
            rect = obj.get_bound_rect()
            if rect.is_inside(click_pos):
                return obj
        return None

    def delete_object(self, obj):
        self.objects.remove(obj)

    def get_selection(self, rect):
        print("========= get_selection:", len(self.objects))
        selection = []

        for obj in self.objects:
            print(" Rect(", rect.left, ",", rect.top, ",",
                  rect.right, ",", rect.bottom, ") ===",
                  obj.get_pos().x, ",", obj.get_pos().y)

            if rect.is_inside(obj.get_pos()):
                selection.append(obj)

        return selection

    def get_objects(self):
        return self.objects

    def add_object(self, obj):
        self.objects.append(obj)

    def add_control_point(self, obj):
        self.control_points.append(obj)

    def delete_control_points(self):
        self.control_points.clear()

    def get_object_index(self, needle):
        for idx, obj in self.objects:
            if obj == needle:
                return idx
        return -1

    def move_to(self, obj, height):
        # FIXME: Implement me
        pass

    def raise_object(self, obj):
        i = self.get_object_index(obj)
        if i != -1 and len(self.objects) > 1 and i < len(self.objects) - 1:
            self.objects[i], self.objects[i + 1] = self.objects[i + 1], self.objects[i]

    def lower_object(self, obj):
        i = self.get_object_index(obj)
        if i != -1 and i > 0:
            self.objects[i], self.objects[i - 1] = self.objects[i - 1], self.objects[i]

# EOF #
