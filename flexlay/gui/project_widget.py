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


from PyQt5.QtCore import Qt
from PyQt5.QtGui import QIcon, QStandardItem
from PyQt5.QtWidgets import (
    QFileSystemModel,
    QFormLayout,
    QLabel,
    QMenu,
    QToolBar,
    QTreeView,
    QVBoxLayout,
    QWidget,
)

from flexlay.util import Signal


class ProjectWidget(QWidget):
    """
    A widget for displaying & editing properties of objects etc.

    Also see the properties this likes to display:
    also see: supertux/property.py
    """

    def __init__(self, parent) -> None:
        super().__init__(parent)
        self.items = []
        self.addon = None

        self.vbox = QVBoxLayout()
        self.vbox.setSpacing(0)
        self.vbox.setContentsMargins(0, 0, 0, 0)

        self.heading_label = QLabel("No project")
        self.label = QLabel("Create a project by selecting File > New > Project...")
        self.vbox.addWidget(self.heading_label)
        self.vbox.addWidget(self.label)
        self.setLayout(self.vbox)

    def init_gui(self) -> None:
        # Clear from previous:
        self.heading_label.setVisible(False)
        self.label.setVisible(False)

        self.toolbar = QToolBar()
        self.toolbar.setStyleSheet('QToolBar{spacing:0px;}')
        package_icon = QIcon("data/images/icons16/addon_package-16.png")
        add_icon = QIcon("data/images/supertux/plus.png")
        self.toolbar.addAction(package_icon, 'Package add-on...', self.package_addon)
        self.toolbar.addAction(add_icon, "Add content...", self.add_content)

        self.tree_view = QTreeView()
        self.vbox.addWidget(self.toolbar)
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
        # self.add_items(self.model, self.data)
        self.tree_view.setModel(self.model)
        self.tree_view.doubleClicked.connect(self.on_tree_view_double_click)
        self.tree_view.setContextMenuPolicy(Qt.CustomContextMenu)
        self.tree_view.customContextMenuRequested.connect(self.on_context_menu)
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

    def call(self) -> None:
        self.call_signal(*self.get_values())

    def on_tree_view_double_click(self, item) -> None:
        print("double-clicked!")

    def on_context_menu(self, position):

        menu = QMenu()
        menu.addAction(self.tr("Add image..."))
        menu.addAction(self.tr("Add sound..."))
        menu.addAction(self.tr("Add level..."))
        menu.addAction(self.tr("Add script..."))

        menu.exec_(self.tree_view.viewport().mapToGlobal(position))

    def set_project_directory(self, project_dir):
        self.tree_view.setRootIndex(self.model.setRootPath(project_dir))

    def set_addon(self, addon):
        self.addon = addon
        # We now have an add-on set, initialize the GUI
        self.init_gui()

    def package_addon(self) -> None:
        print("Package add-on!")

    def add_content(self) -> None:
        print("Add content to add-on!")


# EOF #
