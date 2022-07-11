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


from PyQt5.QtWidgets import QWidget, QLabel, QStatusBar

from flexlay.math.point import Point


class StatusBar:

    def __init__(self, widget: QStatusBar) -> None:
        self.widget = widget
        self.mouse_coordinate_label = QLabel()
        self.mouse_coordinate_label.setText("99, 100")
        self.widget.addPermanentWidget(self.mouse_coordinate_label)

    def clear_message(self) -> None:
        self.widget.clearMessage()

    def show_message(self, text: str) -> None:
        self.widget.showMessage(text)

    def set_mouse_coordinates(self, pos: Point) -> None:
        self.mouse_coordinate_label.setText("(%d, %d)" % (pos.x, pos.y))


# EOF #
