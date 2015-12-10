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


from PyQt4.QtCore import Qt
from PyQt4.QtGui import QFileDialog


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

    def set_directory(self, path):
        self.filename = path
        self.file_dialog.setDirectory(path)


class OpenFileDialog(FileDialog):
    def __init__(self, *args):
        super().__init__(*args)

        self.file_dialog.setAcceptMode(QFileDialog.AcceptOpen)
        self.file_dialog.setFileMode(QFileDialog.ExistingFile)


class SaveFileDialog(FileDialog):
    def __init__(self, *args):
        super().__init__(*args)
        self.file_dialog.setAcceptMode(QFileDialog.AcceptSave)
        self.file_dialog.setFileMode(QFileDialog.AnyFile)

# EOF #
