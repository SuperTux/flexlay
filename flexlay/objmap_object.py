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


class ObjMapObject:

    def __init__(self, pos, metadata):
        self.pos = pos
        self.metadata = metadata
        self.on_select = None
        self.on_deselect = None
        self.on_move = None

    def get_pos(self):
        return self.pos

    def set_pos(self, pos):
        self.pos = pos

    def get_metadata(self):
        return self.metadata

    def set_metadata(self, metadata):
        self.metadata = metadata

    def sig_select(self):
        return self.on_select

    def sig_deselect(self):
        return self.on_deselect

    def sig_move(self):
        return self.on_move


# EOF #
