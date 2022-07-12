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


from typing import Optional

import sys
import logging

from PyQt5.QtWidgets import QApplication

from flexlay.gui_manager import GUIManager


class Flexlay:

    def __init__(self, app_name: str) -> None:
        self.name: str = app_name

        self.application: QApplication = QApplication(sys.argv)
        self.gui_manager: Optional[GUIManager] = None

        logging.basicConfig(format="[%(levelname)s] " + self.name + " %(pathname)s:%(lineno)s: %(message)s")
        logging.getLogger().setLevel(logging.DEBUG)

    def create_gui_manager(self, title: str = "") -> GUIManager:
        if title == "":
            title = self.name
        self.gui_manager = GUIManager(title)
        return self.gui_manager


# EOF #
