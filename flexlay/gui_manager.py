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


from PyQt5.QtCore import (QCoreApplication, Qt)
from PyQt5.QtWidgets import (QApplication, QMainWindow, QToolBar,
                             QDockWidget, QVBoxLayout, QWidget)

from .gui import (ButtonPanel, EditorMapComponent, FileDialog, GenericDialog,
                  LayerSelector, Menubar, Minimap, ObjectSelector,
                  TileBrushSelector, TileSelector)


class GUIManager:

    def __init__(self, title):
        self.window = QMainWindow()
        self.window.setWindowTitle(title)

    def run(self):
        self.window.show()
        QApplication.instance().exec_()

    def quit(self):
        QCoreApplication.quit()

    def create_menubar(self):
        menubar = self.window.menuBar()
        menubar.show()
        return Menubar(menubar)

    def create_button_panel(self, horizontal):
        toolbar = QToolBar()
        if horizontal:
            self.window.addToolBar(Qt.TopToolBarArea, toolbar)
        else:
            self.window.addToolBar(Qt.LeftToolBarArea, toolbar)
        return ButtonPanel(toolbar)

    def create_generic_dialog(self, title):
        return GenericDialog(title, self.window)

    def create_editor_map_component(self, tabbed=True):
        central = QWidget()
        editor = EditorMapComponent(tabbed)
        layout = QVBoxLayout()

        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(editor.get_widget())
        central.setLayout(layout)

        self.window.setCentralWidget(central)

        return editor

    def create_minimap(self, parent):
        dockwidget = QDockWidget("Minimap")
        minimap = Minimap(parent)
        dockwidget.setWidget(minimap.get_widget())

        self.window.addDockWidget(Qt.BottomDockWidgetArea, dockwidget)
        return minimap

    def create_filedialog(self, titel, ok_label, cancel_label):
        return FileDialog(titel, ok_label, cancel_label)

    def create_object_selector(self, w, h):
        dockwidget = QDockWidget("Object Selector")
        object_selector = ObjectSelector(w, h, None)
        dockwidget.setWidget(object_selector.get_widget())

        # self.window.tabifyDockWidget(first, second)
        self.window.addDockWidget(Qt.RightDockWidgetArea, dockwidget)
        return object_selector

    def create_tile_selector(self):
        dockwidget = QDockWidget("Tile Selector")
        tile_selector = TileSelector()
        dockwidget.setWidget(tile_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, dockwidget)
        return tile_selector

    def create_tile_brush_selector(self):
        dockwidget = QDockWidget("Tile Brush")
        tile_brush_selector = TileBrushSelector()
        dockwidget.setWidget(tile_brush_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, dockwidget)
        return tile_brush_selector

    def create_layer_selector(self):
        dockwidget = QDockWidget("Layer Selector")
        layer_selector = LayerSelector()
        dockwidget.setWidget(layer_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, dockwidget)
        return layer_selector


# EOF #
