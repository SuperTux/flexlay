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


from typing import Callable, Optional

from PyQt5.QWidgets import QToolBar
from PyQt5.QtGui import QIcon

from flexlay.gui.icon import Icon


class ButtonPanel:

    def __init__(self, toolbar: QToolBar) -> None:
        self.toolbar = toolbar

    def add_icon(self,
                 filename: str, callback: Callable[[], None],
                 hover: str = "Hover Text",
                 shortcut: Optional[str] = None) -> Icon:
        action = self.toolbar.addAction(QIcon(filename), hover)
        if shortcut is not None:
            action.setShortcut(shortcut)
        if callback:
            action.triggered.connect(lambda checked: callback())
        return Icon(action)

    def add_text(self, name: str, callback: Callable[[], None]) -> Icon:
        action = self.toolbar.addAction(name)
        if callback:
            action.triggered.connect(lambda checked: callback())
        return Icon(action)

    def add_separator(self) -> None:
        self.toolbar.addSeparator()

    def show(self, visible: bool) -> None:
        pass


# EOF #
