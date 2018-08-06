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


from PyQt5.QtWidgets import (QDialog, QDialogButtonBox, QVBoxLayout,
                             QFormLayout)

from flexlay.gui.properties_widget import PropertiesWidget


class GenericDialog(PropertiesWidget):
    """A PropertiesWidget in a QDialog."""

    def __init__(self, title, parent=None):
        super().__init__(parent)

        self.ok_callback = None

        self.dialog = QDialog(parent)

        self.dialog.setModal(True)
        self.dialog.setWindowTitle(title)
        self.buttonbox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)

        vbox = QVBoxLayout()
        self.layout = QFormLayout()
        vbox.addLayout(self.layout)
        vbox.addWidget(self)
        vbox.addWidget(self.buttonbox)

        self.dialog.setLayout(vbox)
        self.dialog.setMinimumWidth(300)
        self.dialog.show()

        # "Ok" and "Cancel" signals

        def on_accept():
            self.call()
            self.dialog.hide()

        def on_rejected():
            self.dialog.hide()

        self.buttonbox.accepted.connect(on_accept)
        self.buttonbox.rejected.connect(on_rejected)


# EOF #
