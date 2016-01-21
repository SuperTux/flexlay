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


from PyQt4.QtCore import (QCoreApplication, Qt)
from PyQt4.QtGui import (QApplication, QMainWindow, QToolBar,
                         QDockWidget, QVBoxLayout, QWidget,
                         QPushButton, QTabWidget)

from flexlay.gui.project_widget import ProjectWidget
from flexlay.gui.properties_widget import PropertiesWidget
from .util.config import Config
from .gui import (ButtonPanel, EditorMapComponent, OpenFileDialog,
                  SaveFileDialog, GenericDialog, LayerSelector,
                  Menubar, Minimap, ObjectSelector, TileBrushSelector,
                  TileSelector, StatusBar)


class FlexlayMainWindow(QMainWindow):
    on_close = None

    def set_on_close(self, on_close):
        self.on_close = on_close

    def closeEvent(self, event):
        if self.on_close:
            if not self.on_close(event) == False:
                Config.current.geometry = self.saveGeometry().toBase64().data().decode()
                Config.current.window_state = self.saveState().toBase64().data().decode()
                event.accept()
            else:
                event.ignore()
        else:
            Config.current.geometry = self.saveGeometry().toBase64().data().decode()
            Config.current.window_state = self.saveState().toBase64().data().decode()
            event.accept()


class GUIManager:
    def __init__(self, title):
        self.window = FlexlayMainWindow()
        self.window.setWindowTitle(title)

        self.editormap_component = None
        self.statusbar = None

        self.tile_selector = None
        self.object_selector = None
        self.tile_selector_dock = None
        self.object_selector_dock = None
        self.layer_selector = None

    def run(self):
        if self.statusbar and self.editormap_component:
            (self.editormap_component.editormap_widget
             .sig_mouse_move.connect(self.statusbar.set_mouse_coordinates))

        if self.tile_selector_dock and self.object_selector_dock:
            self.window.tabifyDockWidget(self.tile_selector_dock,
                                         self.object_selector_dock)

        self.window.show()
        QApplication.instance().exec_()

    def quit(self):
        QCoreApplication.quit()

    def create_statusbar(self):
        self.statusbar = StatusBar(self.window.statusBar())
        return self.statusbar

    def create_menubar(self):
        menubar = self.window.menuBar()
        return Menubar(menubar)

    def create_button_panel(self, horizontal):
        toolbar = QToolBar()
        if horizontal:
            toolbar.setObjectName("button_panel")
        else:
            toolbar.setObjectName("toolbox")
        if horizontal:
            self.window.addToolBar(Qt.TopToolBarArea, toolbar)
        else:
            self.window.addToolBar(Qt.LeftToolBarArea, toolbar)
        return ButtonPanel(toolbar)

    def create_generic_dialog(self, title):
        return GenericDialog(title, self.window)

    def create_editor_map_component(self, tabbed=True):
        central = QWidget()
        self.editormap_component = EditorMapComponent(tabbed)
        layout = QVBoxLayout()

        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.editormap_component.get_widget())
        central.setLayout(layout)

        self.window.setCentralWidget(central)

        return self.editormap_component

    def create_minimap(self, parent):
        dockwidget = QDockWidget("Minimap")
        dockwidget.setObjectName("minimap")
        minimap = Minimap(parent)
        dockwidget.setWidget(minimap.get_widget())

        self.window.addDockWidget(Qt.BottomDockWidgetArea, dockwidget)
        return minimap

    def create_object_selector(self, w, h):
        self.object_selector_dock = QDockWidget("Object Selector")
        self.object_selector_dock.setObjectName("object_selector_dock")
        self.object_selector = ObjectSelector(w, h, None)
        self.object_selector_dock.setWidget(self.object_selector.get_widget())

        # self.window.tabifyDockWidget(first, second)
        self.window.addDockWidget(Qt.RightDockWidgetArea, self.object_selector_dock)
        return self.object_selector

    def create_properties_view(self):
        self.properties_dock = QDockWidget("Properties")
        self.properties_dock.setObjectName("properties_dock")

        self.properties_widget = PropertiesWidget(self.window)
        self.project_widget = ProjectWidget(self.window)

        layout = QVBoxLayout()

        layout.addWidget(self.properties_widget)
        apply = QPushButton("Apply")
        layout.addWidget(apply)
        apply.clicked.connect(self.properties_widget.call)

        widget = QTabWidget()
        widget.addTab(self.properties_widget, "Properties")
        widget.addTab(self.project_widget, "Project View")
        widget.TabPosition = QTabWidget.South
        self.properties_dock.setWidget(widget)

        self.window.addDockWidget(Qt.RightDockWidgetArea, self.properties_dock)
        return self.properties_dock

    def create_tile_selector(self):
        self.tile_selector_dock = QDockWidget("Tile Selector")
        self.tile_selector_dock.setObjectName("tile_selector_dock")
        self.tile_selector = TileSelector()
        self.tile_selector_dock.setWidget(self.tile_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, self.tile_selector_dock)
        return self.tile_selector

    def create_tile_brush_selector(self):
        dockwidget = QDockWidget("Tile Brush")
        tile_brush_selector = TileBrushSelector()
        dockwidget.setWidget(tile_brush_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, dockwidget)
        return tile_brush_selector

    def create_layer_selector(self, generate_tilemap_obj):
        dockwidget = QDockWidget("Layer Selector")
        dockwidget.setObjectName("layer_selector_dock")
        self.layer_selector = LayerSelector(generate_tilemap_obj)
        dockwidget.setWidget(self.layer_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, dockwidget)
        return self.layer_selector

# EOF #
