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

import os

from PyQt4.QtCore import Qt
from PyQt4.QtGui import QFileDialog, QSortFilterProxyModel


class FileDialog:
    def __init__(self, title):
        self.callback = None
        self.file_dialog = QFileDialog()
        self.file_dialog.setWindowModality(Qt.ApplicationModal)
        self.filename = ""

        def on_selected(path):
            self.filename = path

        self.file_dialog.fileSelected.connect(on_selected)

    def run(self, callback):
        self.callback = callback
        if self.callback:
            self.file_dialog.fileSelected.connect(self.callback)

        self.file_dialog.exec_()

    def get_filename(self):
        return self.filename

    def set_directory(self, *dirs):
        path = os.path.join(*dirs)
        self.filename = path
        self.file_dialog.setDirectory(path)


class OpenFileDialog(FileDialog):
    def __init__(self, title, filters=("All Files (*)",)):
        super().__init__(title)

        self.file_dialog.setNameFilters(filters)

        self.file_dialog.setAcceptMode(QFileDialog.AcceptOpen)
        self.file_dialog.setFileMode(QFileDialog.ExistingFile)


class SaveFileDialog(FileDialog):
    def __init__(self, title, default_suffix=""):
        super().__init__(title)

        # FIXME: Not working!?
        self.file_dialog.setDefaultSuffix(default_suffix)

        self.file_dialog.setAcceptMode(QFileDialog.AcceptSave)
        self.file_dialog.setFileMode(QFileDialog.AnyFile)

class OpenDirectoryDialog(OpenFileDialog):
    def __init__(self, title, filters=("All Files (*)",)):
        super().__init__(title, filters)
        self.file_dialog.setFileMode(QFileDialog.Directory)

class SaveDirectoryDialog(SaveFileDialog):
    def __init__(self, title):
        super().__init__(title)
        self.file_dialog.setFileMode(QFileDialog.Directory)

# EOF #
