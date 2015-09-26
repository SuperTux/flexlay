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


class Menu:

    def __init__(self, menu):
        self.menu = menu

    def add_menu(self, label):
        menu = self.menu.addMenu(label)
        return Menu(menu)

    def add_item(self, label, callback):
        action = self.menu.addAction(label)
        if callback:
            action.triggered.connect(lambda *args: callback())
        return action


class Menubar:

    def __init__(self, qmenubar):
        self.qmenubar = qmenubar

    def add_menu(self, label):
        menu = self.qmenubar.addMenu(label)
        return Menu(menu)


# EOF #
