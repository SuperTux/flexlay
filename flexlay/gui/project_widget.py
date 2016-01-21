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

from PyQt4.QtGui import (QVBoxLayout, QLabel, QLineEdit, QFormLayout,
                         QIcon, QCheckBox, QPixmap, QButtonGroup,
                         QRadioButton, QColorDialog, QWidget, QFileDialog,
                         QComboBox, QPushButton, QSpinBox, QTreeView,
                         QStandardItem, QFileSystemModel)

from flexlay.gui import OpenFileDialog
from flexlay.util import Config, Signal

class ProjectWidget(QWidget):
    """
    A widget for displaying & editing properties of objects etc.

    Also see the properties this likes to display:
    also see: supertux/property.py
    """

    def __init__(self, parent):
        super().__init__(parent)
        self.items = []

        self.tree_view = QTreeView()
        self.vbox = QVBoxLayout()
        self.model = QFileSystemModel()
        # self.data = [
        #      ("SuperTux addon", [
        #          ("levels", []),
        #          ("images", []),
        #          ("sounds", []),
        #          ("music", []),
        #          ("scripts", []),
        #          ("metadata", [])
        #      ])]
        # self.model = QStandardItemModel()
        #self.add_items(self.model, self.data)
        self.tree_view.setModel(self.model)
        self.vbox.addWidget(self.tree_view)
        self.layout = QFormLayout()
        self.vbox.addLayout(self.layout)

        self.setLayout(self.vbox)
        self.setMinimumWidth(300)

        # Called in many cases. This should have functions connected
        # which cause the changes in the widget to be applied
        # Called by hitting "Apply", "Ok" or "Finish"
        self.call_signal = Signal()

        self.call_signal.connect(self.call_callbacks)

    def call_callbacks(self, *args):
        for item in self.items:
            if item.callback is not None:
                item.callback(item.get_value())

    def add_callback(self, callback):
        """Adds a callback to the callback signal"""
        self.call_signal.connect(callback)

    def add_items(self, parent, elements):
        for text, children in elements:
            item = QStandardItem(text)
            parent.appendRow(item)
            if children:
                self.add_items(item, children)

    def call(self):
        self.call_signal(*self.get_values())

    def set_project_directory(self, project_dir):
        self.tree_view.setRootIndex(self.model.setRootPath(project_dir))
