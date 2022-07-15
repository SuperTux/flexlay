# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2015 Karkus476 <karkus476@yahoo.com>
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


from flexlay.gui.file_dialog import OpenFileDialog, SaveFileDialog


class OpenLevelFileDialog(OpenFileDialog):

    def __init__(self, title: str) -> None:
        super().__init__(title, ("SuperTux Files (*.stl *.stwm)",
                                 "SuperTux Levels (*.stl)",
                                 "SuperTux Worldmaps (*.stwm)",
                                 "All Files (*)"))


class SaveLevelFileDialog(SaveFileDialog):

    def __init__(self, title: str, is_worldmap: bool = False) -> None:
        super().__init__(title, ".stwm" if is_worldmap else ".stl")


# EOF #
