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


from typing import Callable, Optional

from PyQt5.QtGui import QCloseEvent
from PyQt5.QtCore import (QCoreApplication, Qt)
from PyQt5.QtWidgets import (
    QApplication, QMainWindow, QToolBar,
    QDockWidget, QVBoxLayout, QWidget,
    QPushButton, QTabWidget)

from flexlay.gui.button_panel import ButtonPanel
from flexlay.gui.editor_map_component import EditorMapComponent
from flexlay.gui.generic_dialog import GenericDialog
from flexlay.gui.layer_selector import LayerSelector
from flexlay.gui.menubar import Menubar
from flexlay.gui.minimap import Minimap
from flexlay.gui.object_selector import ObjectSelector
from flexlay.gui.project_widget import ProjectWidget
from flexlay.gui.properties_widget import PropertiesWidget
from flexlay.gui.statusbar import StatusBar
from flexlay.gui.tile_brush_selector import TileBrushSelector
from flexlay.gui.tile_selector import TileSelector
from flexlay.objmap_tilemap_object import ObjMapTilemapObject
from flexlay.util.config import Config


class FlexlayMainWindow(QMainWindow):

    on_close = None

    def set_on_close(self, on_close: Callable[[QCloseEvent], bool]) -> None:
        self.on_close = on_close

    def closeEvent(self, event: QCloseEvent) -> None:
        assert Config.current is not None

        if self.on_close:
            if self.on_close(event):
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

    def __init__(self, title: str) -> None:
        self.window = FlexlayMainWindow()
        self.window.setWindowTitle(title)

        self.editormap_component: Optional[EditorMapComponent] = None
        self.statusbar: Optional[StatusBar] = None

        self.tile_selector: Optional[TileSelector] = None
        self.object_selector: Optional[ObjectSelector] = None
        self.tile_selector_dock: Optional[QDockWidget] = None
        self.object_selector_dock: Optional[QDockWidget] = None
        self.layer_selector: Optional[LayerSelector] = None
        self.properties_widget: Optional[PropertiesWidget] = None

    def run(self) -> None:
        if self.statusbar and self.editormap_component:
            self.editormap_component.editormap_widget.sig_mouse_move.connect(self.statusbar.set_mouse_coordinates)

        if self.tile_selector_dock and self.object_selector_dock:
            self.window.tabifyDockWidget(self.tile_selector_dock,
                                         self.object_selector_dock)

        self.window.show()
        app = QApplication.instance()
        assert app is not None
        app.exec()

    def quit(self) -> None:
        QCoreApplication.quit()

    def create_statusbar(self) -> StatusBar:
        self.statusbar = StatusBar(self.window.statusBar())
        return self.statusbar

    def create_menubar(self) -> Menubar:
        menubar = self.window.menuBar()
        return Menubar(menubar)

    def create_button_panel(self, horizontal: bool) -> ButtonPanel:
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

    def create_generic_dialog(self, title: str) -> GenericDialog:
        return GenericDialog(title, self.window)

    def create_editor_map_component(self, tabbed: bool = True) -> EditorMapComponent:
        central = QWidget()
        self.editormap_component = EditorMapComponent(tabbed)
        layout = QVBoxLayout()

        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.editormap_component.get_widget())
        central.setLayout(layout)

        self.window.setCentralWidget(central)

        return self.editormap_component

    def create_minimap(self, parent: EditorMapComponent) -> Minimap:
        dockwidget = QDockWidget("Minimap")
        dockwidget.setObjectName("minimap")
        minimap = Minimap(parent)
        dockwidget.setWidget(minimap.get_widget())

        self.window.addDockWidget(Qt.BottomDockWidgetArea, dockwidget)
        return minimap

    def create_object_selector(self, w: int, h: int) -> ObjectSelector:
        self.object_selector_dock = QDockWidget("Object Selector")
        self.object_selector_dock.setObjectName("object_selector_dock")
        self.object_selector = ObjectSelector(w, h, None)
        self.object_selector_dock.setWidget(self.object_selector.get_widget())

        # self.window.tabifyDockWidget(first, second)
        self.window.addDockWidget(Qt.RightDockWidgetArea, self.object_selector_dock)
        return self.object_selector

    def create_properties_view(self) -> PropertiesWidget:
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
        widget.setTabPosition(QTabWidget.South)
        self.properties_dock.setWidget(widget)

        self.window.addDockWidget(Qt.RightDockWidgetArea, self.properties_dock)

        return self.properties_widget

    def create_tile_selector(self) -> TileSelector:
        self.tile_selector_dock = QDockWidget("Tile Selector")
        self.tile_selector_dock.setObjectName("tile_selector_dock")
        self.tile_selector = TileSelector()
        self.tile_selector_dock.setWidget(self.tile_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, self.tile_selector_dock)
        return self.tile_selector

    def create_tile_brush_selector(self) -> TileBrushSelector:
        dockwidget = QDockWidget("Tile Brush")
        tile_brush_selector = TileBrushSelector()
        dockwidget.setWidget(tile_brush_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, dockwidget)
        return tile_brush_selector

    def create_layer_selector(self, generate_tilemap_obj: Callable[[], ObjMapTilemapObject]) -> LayerSelector:
        dockwidget = QDockWidget("Layer Selector")
        dockwidget.setObjectName("layer_selector_dock")
        self.layer_selector = LayerSelector(generate_tilemap_obj)
        dockwidget.setWidget(self.layer_selector.get_widget())

        self.window.addDockWidget(Qt.RightDockWidgetArea, dockwidget)
        return self.layer_selector


# EOF #
