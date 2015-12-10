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


from PyQt4.QtGui import QStandardItemModel, QStandardItem
from PyQt4.QtGui import QWidget, QToolBar, QTreeView, QVBoxLayout

from flexlay.objmap_tilemap_object import ObjMapTilemapObject
from flexlay.tilemap_layer import TilemapLayer
from .editor_map_component import EditorMapComponent


class LayerSelector:
    def __init__(self):
        self.model = QStandardItemModel()
        # self.model.setHorizontalHeaderItem(0, QStandardItem("Visible"))
        self.model.setHorizontalHeaderItem(1, QStandardItem("Layer"))

        self.vbox = QWidget()

        # Use QTreeWidget instead!?
        self.tree_view = QTreeView()
        self.tree_view.setModel(self.model)

        self.toolbar = QToolBar()
        hide_all = self.toolbar.addAction("Hide All", self.hide_all_layers)
        show_all = self.toolbar.addAction("Show All", self.show_all_layers)

        self.layout = QVBoxLayout(self.vbox)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.tree_view)
        self.layout.addWidget(self.toolbar)

        self.editormap = None

    def set_map(self, editormap):
        self.editormap = editormap

        self.model.clear()

        #        for layer in editormap.layers:
        #            if isinstance(layer, TilemapLayer):
        #                self.model.appendRow([QStandardItem("Tile: %s %dx%d" % (layer.m
        #                                                                        layer.w
        #            elif isinstance(layer, ObjectLayer):
        #                self.model.appendRow([QStandardItem("Objects")])


        # As TilemapLayers are used by ObjMapTilemapObjects,
        # which are stored in the objects array in an ObjectLayer. (!)
        for object in editormap.layers[0].objects:
            if isinstance(object, ObjMapTilemapObject):
                layer = object.tilemap_layer
                if isinstance(layer, TilemapLayer):
                    self.model.appendRow([QStandardItem("Tile: %s %dx%d" % (layer.metadata.name,
                                                                            layer.width, layer.height))])

    def hide_all_layers(self):
        if not self.editormap:
            return
        # Get TilemapLayer, and set hidden
        for object in self.editormap.layers[0].objects:
            if isinstance(object, ObjMapTilemapObject):
                layer = object.tilemap_layer
                if isinstance(layer, TilemapLayer):
                    layer.hidden = True
        # Repaint so that changes are visible
        EditorMapComponent.current.editormap_widget.repaint()

    def show_all_layers(self):
        if not self.editormap:
            return
        # Get TilemapLayer, and set hidden
        for object in self.editormap.layers[0].objects:
            if isinstance(object, ObjMapTilemapObject):
                layer = object.tilemap_layer
                if isinstance(layer, TilemapLayer):
                    layer.hidden = False
        # Repaint so that changes are visible
        EditorMapComponent.current.editormap_widget.repaint()

    def get_widget(self):
        return self.vbox

# EOF #
