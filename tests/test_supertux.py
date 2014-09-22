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
from supertux.level import Level

import os
import io
import unittest
import unittest.mock

test_levelfile = os.path.join(os.path.dirname(__file__), "test.stl")


class SuperTuxTestCase(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_level_load(self):
        Config.create("supertux-editor")
        level = Level.from_file(test_levelfile)
        self.assertEqual(level.name, "Welcome to Antarctica")

    def test_level_save(self):
        Config.create("supertux-editor")
        level = Level.from_file(test_levelfile)
        with io.StringIO() as out:
        # with open("/tmp/test.stl", "w") as out:
            level.save_io(out)
            # print(out.getvalue())

    def test_level_new(self):
        level = Level.from_size(400, 300)
        self.assertEqual(level.name, "No Name")


if __name__ == '__main__':
    unittest.main()


# EOF #
