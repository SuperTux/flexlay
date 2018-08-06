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


class Surface:

    def __init__(self, pixelbuffer):
        pass

    def draw_stretched(self, rect):
        pass

    def draw(self, x, y):
        pass

    def set_alignment(self, origin, x=0, y=0):
        pass

    def set_alpha(self, alpha):
        pass

    def set_scale(self, x, y):
        pass

    def set_blend_func(self, src, dest):
        pass

    @property
    def width(self):
        return 0

    @property
    def height(self):
        return 0


# EOF #
