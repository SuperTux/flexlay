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


from flexlay.util import get_value_from_tree, sexpr_filter, SExprWriter

from .sector import Sector
from .util import load_lisp


class Level:

    @staticmethod
    def from_file(filename):
        level = Level()

        level.filename = filename

        tree = load_lisp(level.filename, "supertux-level")
        data = tree[1:]

        level.version = get_value_from_tree(["version", "_"], data, 0)

        print("VERSION:", level.filename, " ",  level.version)

        if level.version == 1:
            raise Exception("version 1 levels not supported at the moment")
        else:
            level.parse_v2(data)

        return level

    @staticmethod
    def from_size(width, height):
        result = Level()
        result.current_sector = Sector(result)
        result.current_sector.new_from_size("main", width, height)
        result.sectors.append(result.current_sector)
        return result

    def __init__(self):
        self.version = 2
        self.filename = None
        self.name = "No Name"
        self.author = "No Author"
        self.license = "GPL 2+ / CC-by-sa 3.0"
        self.target_time = 0

        self.current_sector = None
        self.sectors = []

    def parse_v2(self, data):
        self.name = get_value_from_tree(["name", "_"], data, "no name")
        self.author = get_value_from_tree(["author", "_"], data, "no author")
        self.license = get_value_from_tree(["license", "_"], data, "no license")
        self.target_time = get_value_from_tree(["target-time", "_"], data, 0)

        self.current_sector = None
        self.sectors = []
        for sec in sexpr_filter("sector", data):
            sector = Sector(self)
            sector.load_v2(sec)
            self.sectors.append(sector)
            if sector.name == "main":
                self.current_sector = sector

        if self.current_sector is None:
            print("Error: No main sector defined:", self.sectors)
            self.current_sector = self.sectors[0]

    def save(self, filename):
        self.save_v2(filename)

    def save_v2(self, filename):
        with open(filename, "w") as f:
            self.save_io(f)

    def save_io(self, f):
        writer = SExprWriter(f)
        writer.begin_list("supertux-level")
        writer.write_int("version", 2)
        writer.write_tr_string("name", self.name)
        writer.write_string("author", self.author)
        writer.write_int("target-time", self.target_time)
        writer.write_string("license", self.license)

        for sector in self.sectors:
            writer.begin_list("sector")
            sector.save(writer)
            writer.end_list()

        writer.end_list()

    def add_sector(self, sector):
        self.sectors.append(sector)

    def remove_sector(self, name):
        if len(self.sectors) > 1:
            self.sectors = [sec for sec in self.sectors if sec.name != name]
        else:
            print("Only one sector left, can't delete it")

    def get_sectors(self):
        return [sec.name for sec in self.sectors]


# EOF #
