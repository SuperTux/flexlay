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


from PyQt5.QtGui import (QStandardItemModel, QStandardItem, QIcon)
from PyQt5.QtWidgets import (QWidget, QToolBar, QTreeView, QVBoxLayout)
from flexlay.tool_context import ToolContext
from flexlay.commands import LayerDeleteCommand
from .editor_map_component import EditorMapComponent
from ..objmap_tilemap_object import ObjMapTilemapObject
from ..tilemap_layer import TilemapLayer
from ..util.signal import Signal


class LayerTreeView(QTreeView):
    """A QTreeView for layers"""
    def __init__(self, layer_selector, parent=None, selection_callback=None):
        """Initialises a LayerTreeView

        Only difference from QTreeView is that a Signal is created
        to handle whenever the selection changes.
        :param: selection_callback: A callback to be connected immediately
        """
        super().__init__(parent)
        # Signal called when selection changes
        self.selection_signal = Signal()
        if selection_callback:
            self.selection_signal.connect(selection_callback)
        # The layers selector object which should have created this LayerTreeView
        self.layer_selector = layer_selector

    def selectionChanged(self, selected, deselected):
        """Called when user clicks on a different Layer

        QTreeView function, not to be called by anything else.

        :param selected - QItemSelection previously selected
        :param deselected - QItemSelection previously deselected

        Calls all callbacks in selection_signal

        With parameters as indexes in tree view (row which is selected, top is 0):
        selected : What's now selected. None if nothing is selected.
        deselected  What used to be selected. None if nothing was selected
        """
        super().selectionChanged(selected, deselected)

        selected_index = None
        deselected_index = None

        if len(selected.indexes()) > 0:
            selected_index = selected.indexes()[0].row()
        if len(deselected.indexes()) > 0:
            deselected_index = deselected.indexes()[0].row()

        self.selection_signal(selected_index, deselected_index)

    def dataChanged(self, top_left, bottom_right, roles=[]):
        """Overrides in QTreeView. Ensures name of actual tilemap is set"""
        super().dataChanged(top_left, bottom_right, roles)
        if top_left == bottom_right:
            index = top_left.row()
            data = str(top_left.data())
            self.layer_selector.get_layers()[index].name = data


class LayerSelector:
    """Show layers in a Tree View to be selected

    Also handles selected layer. Although more than one may be selected,
    only caters for a single one being selected. (most likely first
    one selected, but don't rely on that.

    **NOTE** When hiding layers, use set_hidden or toggle_hidden here
             When getting (a) layer(s) use get_layers and get_layer to
             return a TilemapLayer
    """
    def __init__(self, generate_tilemap_obj):
        """A way to view layers

        :param metadata_from_size: A method/function which returns metadata
                                   for an ObjMapTilemapObject with arguments
                                   width, height
        """
        self.model = QStandardItemModel()
        # QStandardItems in the model (to set font etc.)
        # items are added to list by self.set_map()
        self.items = []
        # Preferably use get_layers and get_layer
        self.tilemap_layers = []
        # self.model.setHorizontalHeaderItem(0, QStandardItem("Visible"))
        self.model.setHorizontalHeaderItem(1, QStandardItem("Layer"))

        self.vbox = QWidget()

        # Use QTreeWidget instead!?
        self.tree_view = LayerTreeView(self, selection_callback=self.selection_changed)
        self.tree_view.setModel(self.model)

        self.toolbar = QToolBar()

        self.toolbar.addAction("Hide All", self.hide_all)
        self.toolbar.addAction("Show All", self.show_all)

        # Eye icons:
        self.eye_open_icon = QIcon("data/images/supertux/stock-eye-12.png")
        self.eye_closed_icon = QIcon("data/images/supertux/stock-eye-half-12.png")
        # Button to toggle selected layer hidden/shown.
        self.current_hidden = self.toolbar.addAction(self.eye_open_icon,
                                                     "Toggle Visibility",
                                                     self.toggle_action)
        # Stays pressed when clicked. Pressed = hidden, else shown
        self.current_hidden.setCheckable(True)

        # Buttons to add/remove layers.
        self.toolbar.addAction(QIcon("data/images/supertux/plus.png"),
                               "New Layer",
                               self.add_layer)
        self.toolbar.addAction(QIcon("data/images/supertux/minus.png"),
                               "Delete This Layer",
                               self.remove_current_layer)

        self.layout = QVBoxLayout(self.vbox)
        self.layout.setContentsMargins(0, 0, 0, 0)
        self.layout.addWidget(self.tree_view)
        self.layout.addWidget(self.toolbar)

        # Currently selected index, -1 if nothing selected
        self.selected_index = -1

        # Show only selected layer if true
        self.show_only_selected = False

        self.generate_tilemap_obj = generate_tilemap_obj

        # To get the tilemap_layers
        self.editormap = None

    def toggle_hidden(self, index):
        """Run set hidden on selected tilemap to toggle visibility"""
        if self.selected_index >= 0:
            self.set_hidden(self.selected_index, not self.is_hidden(self.selected_index))

    def set_hidden(self, index, hidden, repaint=True):
        """Set tilemap_layer to hidden

        :param index is the index of the tilemap in the treeview
        :param hidden is a boolean to set it to True = not visible
        :param repaint: Whether to repaint the screen immediately after (for efficiency purposes)
        """
        if len(self.items) > index:
            # Get font of relevant item
            font = self.items[index].font()
            font.setBold(not hidden)
            self.items[index].setFont(font)

        if len(self.get_layers()) > index:
            self.get_layer(index).hidden = hidden

        if repaint:
            EditorMapComponent.current.editormap_widget.repaint()

    def is_hidden(self, index):
        """Returns True if tilemap_layer at index

        in hidden is hidden else False
        """
        # Check for None and False.
        tilemap_layer = self.get_layer(index)
        if not tilemap_layer:
            return False
        return tilemap_layer.hidden

    def set_map(self, editormap):
        """Refresh, showing new layers in tree view"""
        self.editormap = editormap

        self.model.clear()
        self.items = []

        # When done this way, we can expect that the position in the
        # TreeView corresponds to poistion in list.
        unnamed_count = 1
        for layer in self.get_layers():
            if layer.metadata.name == "":
                standard_item = QStandardItem("No name (" + str(unnamed_count) + ")")
                unnamed_count+=1
            else:
                standard_item = QStandardItem(layer.metadata.name)
            self.model.appendRow([standard_item])
            self.items.append(standard_item)

        # Ensure all are visible and set to bold.
        self.show_all()

    def hide_all(self):
        """Hide all layers in this editormap"""
        if not self.editormap:
            return
        # Get TilemapLayer, and set hidden
        for i in range(len(self.get_layers())):
            self.set_hidden(i, True, repaint=False)
        # Repaint so that changes are visible
        EditorMapComponent.current.editormap_widget.repaint()

    def show_all(self):
        """Unhide all layers in this editormap"""
        if not self.editormap:
            return
        # Get TilemapLayer, and set hidden
        for i in range(len(self.get_layers())):
            self.set_hidden(i, False, repaint=False)
        # Repaint so that changes are visible
        EditorMapComponent.current.editormap_widget.repaint()

    def get_widget(self):
        return self.vbox

    def get_layers(self):
        """Returns all the tilemap_layers associated with current editormap"""
        return self.editormap.get_tilemap_layers()

    def get_layer(self, index):
        """Gets tilemap from the list of tilemaps
        :param index: Which tilemap to get
        :return: TilemapLayer from tilemaps. None if invalid index
        """
        if index is not None:
            try:
                return self.get_layers()[index]
            except IndexError:
                pass
        return None

    def toggle_current(self):
        """Toggle the currently selected layer visibility"""
        self.toggle_hidden(self.selected_index)

    def toggle_show_only_selected(self):
        self.show_only_selected = not self.show_only_selected

    def toggle_action(self):
        """Toggle current layer and change eye icon to open/close"""
        self.toggle_current()
        if self.is_hidden(self.selected_index):
            self.current_hidden.setIcon(self.eye_closed_icon)
        else:
            self.current_hidden.setIcon(self.eye_open_icon)

    def get_selected(self):
        """Returns TilemapLayer which is currently selected, if any"""
        return self.get_layer(self.selected_index)

    def selection_changed(self, selected, deselected):
        """Connected to LayerTreeView selectionChanged"""
        self.selected_index = selected
        tilemap_layer = self.get_selected()
        if tilemap_layer:
            ToolContext.current.tilemap_layer = tilemap_layer
            if self.show_only_selected and selected:
                self.hide_all()
                self.set_hidden(selected, True)

            # Set toggle button
            self.current_hidden.setChecked(tilemap_layer.hidden)
            if tilemap_layer.hidden:
                self.current_hidden.setIcon(self.eye_closed_icon)
            else:
                self.current_hidden.setIcon(self.eye_open_icon)

    def add_layer(self, tilemap_object=None):
        """Creates a new layer"""
        if tilemap_object is None:
            if self.generate_tilemap_obj is None:
                raise RuntimeError("Layer Selector cannot create tilemaps without metadata")

            tilemap_object = self.generate_tilemap_obj()

        # Add object to editormap
        self.editormap.layers[0].add_object(tilemap_object)

        # Create item
        item = QStandardItem(tilemap_object.tilemap_layer.name)

        # Set bold if required
        font = item.font()
        font.setBold(not tilemap_object.tilemap_layer.hidden)
        item.setFont(font)

        self.items.append(item)

        self.model.appendRow(item)

    def remove_current_layer(self):
        self.remove_layer(self.selected_index)

    def remove_layer(self, layer):
        """Deletes this layer safely, adding to the editormap's undo stack

        :param layer: Either a TilemapLayer, an ObjMapTilemapObject or an int (the layer to remove)
        """
        command = LayerDeleteCommand(self, layer)
        self.editormap.execute(command)

    def unsafe_remove_layer(self, layer):
        """Remove a layer without adding to undo_stack

        :param layer: Either a TilemapLayer, an ObjMapTilemapObject or an int (the layer to remove)
        """
        if isinstance(layer, int):
            tilemap_layer = self.get_layer(layer)
            index = layer
        elif isinstance(layer, TilemapLayer):
            tilemap_layer = layer
            index = self.get_layers().find(tilemap_layer)
        elif isinstance(layer, ObjMapTilemapObject):
            tilemap_layer = layer.tilemap_layer
            index = self.get_layers().index(tilemap_layer)
        else:
            raise RuntimeError("Layer Selector: Cannot pass " + str(type(layer)) + " to _remove_layer\n" +
                               "Try instead: ObjMapTilemapObject, TilemapLayer or int")

        tilemap_object = self.editormap.remove_tilemap_layer(tilemap_layer)
        self.model.removeRow(index)
        # Stop errors
        if self.selected_index == index:
            self.selected_index = -1
            self.tree_view.clearSelection()
        # Update EditorMap
        EditorMapComponent.current.editormap_widget.repaint()

        return tilemap_object

# EOF #
