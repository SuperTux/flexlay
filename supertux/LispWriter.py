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


class LispWriter:

    def __init__(self, file):
        self.file = file
        self.indent_depth = 0

    def write_comment(self, comment):
        self.file.write("; " + comment + "\n")

    def start_list(self, listname):
        self.indent()
        self.file.write("(" + listname + "\n")
        self.indent_depth += 2

    def end_list(self, listname):
        self.indent_depth -= 2
        self.indent()
        self.file.write(")\n")

    def write_int(self, name, value):
        self.indent()
        self.file.write("(%s %d)\n" % (name, value))

    def write_float(self, name, value):
        self.indent()
        self.file.write("(%s %f)\n" % (name, value))

    def write_string(self, name, value, translatable=False):
        self.indent()
        self.file.write("(" + name)
        if translatable:
            self.file.write(" (_ \"" + value + "\"))\n")
        else:
            self.file.write(" \"" + value + "\")\n")

    def write_bool(self, name, value):
        self.indent()
        self.file.write("(" + name + " ")
        if value:
            self.file.write("#t")
        else:
            self.file.write("#f")
            self.file.write(")\n")

    def write_int_vector(self, name, value):
        self.indent()
        self.file.write("(" + name)
        for i in value:
            self.file.write(" %d" % [i])
            self.file.write(")\n")

    def indent(self):
        self.file.write(" " * self.indent_depth)


# EOF #
