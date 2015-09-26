#!/usr/bin/env python3

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


from flexlay import Config
from flexlay.math import Point, Size
from supertux.level import Level
from supertux.gameobj_factor import supertux_gameobj_factory

import os
import io
import unittest
import unittest.mock

test_levelfile = os.path.join(os.path.dirname(__file__), "test.stl")


class SuperTuxTestCase(unittest.TestCase):

    def setUp(self):
        Config.create("supertux-editor")

    def tearDown(self):
        Config.current = None

    def test_level_load(self):
        level = Level.from_file(test_levelfile)
        self.assertEqual(level.name, "Welcome to Antarctica")

    def test_level_save(self):
        level = Level.from_file(test_levelfile)
        # with open("/tmp/test.stl", "w") as out:
        with io.StringIO() as out:
            level.save_io(out)
            # print(out.getvalue())

    def test_level_new(self):
        level = Level.from_size(400, 300)
        self.assertEqual(level.name, "No Name")

    def test_gameobj_factory_create_object_brushes(self):
        supertux_gameobj_factory.create_object_brushes()

    def test_gameobj_factory_create_gameobj_at(self):
        for identifier, (_, _) in supertux_gameobj_factory.objects.items():
            try:
                obj = supertux_gameobj_factory.create_gameobj_at(identifier, Point(0, 0))
            except Exception:
                print("Exception received: %r %r" % (identifier, obj))
                raise

    def test_level_resize(self):
        level = Level.from_file(test_levelfile)
        level.sectors[0].resize(Size(10, 10), Point(10, 10))

if __name__ == '__main__':
    unittest.main()


# EOF #
