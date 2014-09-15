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


from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import QFileDialog


class FileDialog:

    def __init__(self, title, ok_label, cancel_label):
        self.callback = None
        self.file_dialog = QFileDialog()
        self.file_dialog.setFileMode(QFileDialog.ExistingFile)
        self.file_dialog.setWindowModality(Qt.ApplicationModal)

    def run(self, callback):
        self.callback = callback
        if self.callback:
            self.file_dialog.fileSelected.connect(self.callback)

        self.file_dialog.show()

    def get_filename(self):
        pass

    def set_filename(self, file_dialog):
        # GRUMBEL
        pass


# EOF #
