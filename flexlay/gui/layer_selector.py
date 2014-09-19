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


from PyQt4.QtCore import QSize
from PyQt4.QtGui import QStandardItemModel, QStandardItem, QIcon, QSizePolicy
from PyQt4.QtGui import (QWidget, QToolBar, QTreeView, QScrollArea,
                         QVBoxLayout)


class LayerSelector:

    def __init__(self):
        model = QStandardItemModel()
        model.setHorizontalHeaderItem(0, QStandardItem("Visible"))
        model.setHorizontalHeaderItem(1, QStandardItem("Layer"))

        model.appendRow([QStandardItem(QIcon("../data/images/icons16/resize1.png"), "Wrong"),
                         QStandardItem("Background")])

        model.appendRow([QStandardItem("Eye"),
                         QStandardItem("Interactive")])

        model.appendRow([QStandardItem("Eye"),
                         QStandardItem("Foreground")])
        self.vbox = QWidget()

        # Use QTreeWidget instead!?
        self.tree_view = QTreeView()
        self.tree_view.setModel(model)

        self.toolbar = QToolBar()
        self.toolbar.addAction("Hide All")
        self.toolbar.addAction("Show All")

        self.layout = QVBoxLayout(self.vbox)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.tree_view)
        self.layout.addWidget(self.toolbar)

    def get_widget(self):
        return self.vbox

# EOF #
