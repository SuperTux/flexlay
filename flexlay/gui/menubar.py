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


from typing import Callable

from PyQt5.QWidgets import QMenu, QMenuBar
from PyQt5.QGui import QAction


class Menu:

    def __init__(self, menu: QMenu) -> None:
        self.menu = menu

    def add_menu(self, label: str) -> Menu:
        menu = self.menu.addMenu(label)
        return Menu(menu)

    def add_item(self, label: str, callback: Callable[[], None]) -> QAction:
        action = self.menu.addAction(label)
        if callback:
            action.triggered.connect(lambda checked: callback())
        return action


class Menubar:

    def __init__(self, qmenubar: QMenuBar) -> None:
        self.qmenubar = qmenubar

    def add_menu(self, label: str) -> Menu:
        menu = self.qmenubar.addMenu(label)
        return Menu(menu)


# EOF #
