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


from PyQt4.QtGui import QIcon

from .icon import Icon


class ButtonPanel:

    def __init__(self, toolbar):
        self.toolbar = toolbar

    def add_icon(self, filename, callback, hover="Text"):
        action = self.toolbar.addAction(QIcon(filename), hover)
        if callback:
            action.triggered.connect(callback)
        return Icon(action)

    def add_text(self, name, callback):
        action = self.toolbar.addAction(name)
        if callback:
            action.triggered.connect(callback)
        return Icon(action)

    def add_separator(self):
        self.toolbar.addSeparator()

    def show(self, visible):
        pass


# EOF #
