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


from flexlay.math import Point


class Origin:
    """Document me!"""
    top_left = 0
    top_center = 1
    top_right = 2
    center_left = 3
    center = 4
    center_right = 5
    bottom_left = 6
    bottom_center = 7
    bottom_right = 8

    @staticmethod
    def calc_origin(origin, size):
        if origin == Origin.top_left:
            return Point(0, 0)
        elif origin == Origin.top_center:
            return Point(size.width / 2, 0)
        elif origin == Origin.top_right:
            return Point(size.width, 0)
        elif origin == Origin.center_left:
            return Point(0, size.height / 2)
        elif origin == Origin.center:
            return Point(size.width / 2, size.height / 2)
        elif origin == Origin.center_right:
            return Point(size.width, size.height / 2)
        elif origin == Origin.bottom_left:
            return Point(0, size.height)
        elif origin == Origin.bottom_center:
            return Point(size.width / 2, size.height)
        elif origin == Origin.bottom_right:
            return Point(size.width, size.height)

    def __init__(self):
        pass

# EOF #
