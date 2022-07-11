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


from PyQt5.QtWidgets import QListWidget

from flexlay.tile_brush import TileBrush


class TileBrushSelector:

    def __init__(self) -> None:
        self.brushes: list[TileBrush] = []
        self.list_widget = QListWidget()

    def add_brush(self, name: str, brush: TileBrush) -> None:
        self.list_widget.addItem(name)
        self.brushes.append(brush)

    def get_widget(self) -> QListWidget:
        return self.list_widget


# EOF #
