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


import math

from flexlay.math import Rectf


class Dab:

    def __init__(self) -> None:
        self.time = None
        self.pos = None
        self.pressure = None
        self.tilt = None


class Stroke:

    def __init__(self) -> None:
        self.dabs = []
        self.drawer = None
        self.bounding_rect_needs_recalc = True

    def calc_bounding_rect(self) -> Rectf:
        rect = Rectf()

        # FIXME: Keep the drawer into account (ie. brushsize)
        if len(self.dabs) > 0:
            rect.left = rect.right = self.dabs[0].pos.x
            rect.top = rect.bottom = self.dabs[0].pos.y

            for dab in self.dabs[1:]:
                rect.left = min(dab.pos.x, rect.left)
                rect.top = min(dab.pos.y, rect.top)

                rect.right = max(dab.pos.x, rect.right)
                rect.bottom = max(dab.pos.y, rect.bottom)

        return rect

    def set_drawer(self, drawer) -> None:
        self.drawer = drawer

    def get_drawer(self) -> Drawer:
        return self.drawer

    def get_interpolated_dabs(self, x_spacing, y_spacing):
        if len(self.dabs) > 0:
            interpolated_dabs = []

            interpolated_dabs.append(self.dabs[0])

            # The following code basically takes all the event dabs as recieved
            # by from the InputDevice and interpolates new dabs inbetween to
            # give them an equal spacing (ie. every dab is only 'spacing' away
            # from the next)
            overspace = 0.0
            dabs = self.dabs
            for j in range(0, len(dabs) - 1):
                dist = dabs[j + 1].pos - dabs[j].pos
                length = math.sqrt(dist.x * dist.x + dist.y * dist.y)
                n = 1

                # Spacing is keep relative to the brush size
                # FIXME: This is specific to a Sprite based drawer, might not work for others
                # FIXME: y_spacing isn't taken into account either
                local_spacing = x_spacing * dabs[j].pressure

                while length + overspace > (local_spacing * n):
                    factor = (local_spacing / length) * n - (overspace / length)

                    # FIXME: Interpolate tilting, pressure, etc. along the line
                    interpolated_dabs.append(Dab(dabs[j].pos.x + dist.x * factor,
                                                 dabs[j].pos.y + dist.y * factor,
                                                 dabs[j].pressure))
                    n += 1

                # calculate the space that wasn't used in the last iteration
                overspace = (length + overspace) - (local_spacing * (n - 1))

                return interpolated_dabs
        else:
            # No dabs available, so nothing to interpolate
            return self.dabs

    def get_dabs(self):
        return self.dabs

    def get_dab_count(self):
        return self.dabs.size()

    def draw(self, gc):
        if self.drawer:
            self.drawer.draw(self, gc)
        else:
            print("No drawer set!")

    def add_dab(self, dab):
        self.dabs.append(dab)

    def get_bounding_rect(self):
        if self.bounding_rect_needs_recalc:
            self.bounding_rect = self.calc_bounding_rect()
            self.bounding_rect_needs_recalc = False

        return self.bounding_rect

    def empty(self):
        return self.dabs.size() == 0


# EOF #
