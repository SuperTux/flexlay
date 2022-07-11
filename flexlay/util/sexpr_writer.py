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


from typing import Any, IO, Optional, Union


from flexlay.color import Color
from flexlay.math.point import Point, Pointf
from flexlay.math.rect import Rect, Rectf
from flexlay.math.size import Size, Sizef
from flexlay.field import Field


SExprValue = Union[bool, int, float, str, list]


def write_sexpr(fout: IO[str], sexpr: SExprValue, indent: int = 0) -> None:
    if isinstance(sexpr, list):
        fout.write("(\n")
        for i, e in enumerate(sexpr):
            write_sexpr(fout, e)
            if i != len(sexpr) - 1:
                fout.write(" \n")
        fout.write(")\n")
    else:
        # if sexpr.is_a?(Symbol):
        #   fout.write(str(sexpr))
        # else:
        fout.write(repr(sexpr))


class SExprWriter:

    def __init__(self, fout: IO[str]) -> None:
        self.fout = fout
        self.indent_depth = 0

    def write_comment(self, comment: str) -> None:
        self.fout.write(";; " + comment + "\n")

    def begin_list(self, listname: str) -> None:
        self.indent()
        self.fout.write("(" + listname + "\n")
        self.indent_depth += 2

    def end_list(self, listname: Optional[str] = None) -> None:
        self.indent_depth -= 2
        self.indent()
        self.fout.write(")\n")

    def write(self, name: str, value: SExprValue) -> None:
        if isinstance(value, bool):
            self.write_bool(name, value)
        elif isinstance(value, int):
            self.write_int(name, value)
        elif isinstance(value, float):
            self.write_float(name, value)
        elif isinstance(value, str):
            self.write_string(name, value)
        elif isinstance(value, list):
            self.write_vector(name, value)
        else:
            raise RuntimeError("unknown requested in generic write(): %r" % type(value))

    def write_bool(self, name: str, value: bool) -> None:
        self.indent()
        self.fout.write("(" + name + " ")
        if value:
            self.fout.write("#t")
        else:
            self.fout.write("#f")
        self.fout.write(")\n")

    def write_int(self, name: str, value: int) -> None:
        self.indent()
        self.fout.write("(%s %d)\n" % (name, value))

    def write_float(self, name: str, value: float) -> None:
        self.indent()
        # %f makes ugly floats, e.g. 45.000000, %s gives nicely
        # trimmed ones, e.g. "45.0"
        self.fout.write("(%s %s)\n" % (name, value))

    def write_tr_string(self, name: str, value: str) -> None:
        self.write_string(name, value, translatable=True)

    def write_string(self, name: str, value: str, translatable: bool = False) -> None:
        self.indent()
        self.fout.write("(" + name)
        if translatable:
            self.fout.write(" (_ \"" + value.replace('"', '\\"') + "\"))\n")
        else:
            self.fout.write(" \"" + value.replace('"', '\\"') + "\")\n")

    def write_rgb(self, name: str, color: list[float]) -> None:
        self.indent()
        self.fout.write("(" + name)
        self.fout.write(" %s %s %s)\n" % (color[0], color[1], color[2]))

    def write_inline_point(self, pos: Point) -> None:
        self.write_int("x", pos.x)
        self.write_int("y", pos.y)

    def write_inline_pointf(self, pos: Pointf) -> None:
        if pos.x != 0.0:
            self.write_float("x", pos.x)

        if pos.y != 0.0:
            self.write_float("y", pos.y)

    def write_inline_size(self, size: Size) -> None:
        self.write_int("width", size.width)
        self.write_int("height", size.height)

    def write_inline_sizef(self, size: Sizef) -> None:
        self.write_float("width", size.width)
        self.write_float("height", size.height)

    def write_inline_rect(self, rect: Rect) -> None:
        # ugly this way around, but for compatibilty with the C# supertux-editor
        self.write_int("width", rect.width)
        self.write_int("height", rect.height)
        self.write_int("x", rect.left)
        self.write_int("y", rect.top)

    def write_inline_rectf(self, rect: Rectf) -> None:
        # ugly this way around, but for compatibilty with the C# supertux-editor
        self.write_float("width", rect.width)
        self.write_float("height", rect.height)
        self.write_float("x", rect.left)
        self.write_float("y", rect.top)

    def write_vector(self, name: str, values: list[Any]) -> None:
        self.indent()
        self.fout.write("(" + name)
        for i in values:
            self.fout.write(" %r" % i)
        self.fout.write(")\n")

    def write_color(self, name: str, values: 'Color') -> None:
        self.indent()
        self.fout.write("(" + name)
        for i in values:
            if i == 1.0:
                self.fout.write(" 1")
            elif i == 0:
                self.fout.write(" 0")
            else:
                self.fout.write(" %r" % i)
        self.fout.write(")\n")

    def write_field(self, name: str, field: Field) -> None:
        self.indent()
        self.fout.write("(%s\n" % name)
        for y in range(0, field.height):
            self.indent()
            for x in range(0, field.width):
                if x < field.width - 1:
                    self.fout.write("%d " % field.at(x, y))
                else:
                    self.fout.write("%d" % field.at(x, y))
            self.fout.write("\n")
        self.indent()
        self.fout.write(")\n")

    def indent(self) -> None:
        self.fout.write(" " * self.indent_depth)


# EOF #
