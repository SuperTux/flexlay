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


import sys
import logging

from PyQt4.QtGui import QApplication

from .gui_manager import GUIManager


class Flexlay:
    def __init__(self, app_name):
        self.name = app_name

        self.application = QApplication(sys.argv)
        self.gui_manager = None

        logging.basicConfig(format="[%(levelname)s] "+ self.name + " %(pathname)s:%(lineno)s: %(message)s")
        logging.getLogger().setLevel(logging.DEBUG)

    def create_gui_manager(self, title=""):
        if title == "":
            title = self.name
        self.gui_manager = GUIManager(title)
        return self.gui_manager

# EOF #
