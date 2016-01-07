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
                         QComboBox, QPushButton, QSpinBox)

from flexlay.gui import OpenFileDialog
from flexlay.util import Config, Signal


class Item:
    KIND_LABEL = 0
    KIND_BOOL = 1
    KIND_INT = 2
    KIND_FLOAT = 3
    KIND_STRING = 4
    KIND_ENUM = 5
    KIND_RADIO = 6
    KIND_COLOR = 7

    def __init__(self, kind, label, body, callback=None, group=None):
        self.kind = kind
        self.label = label
        self.body = body
        self.group = group
        self.callback = None

    def get_value(self):
        if self.kind == Item.KIND_LABEL:
            return None

        elif self.kind == Item.KIND_RADIO:
            idx = 0
            for button in self.group.buttons():
                if button == self.group.checkedButton():
                    return idx
                idx += 1

        elif self.kind == Item.KIND_ENUM:
            return self.body.currentText()

        elif self.kind == Item.KIND_BOOL:
            return self.body.checkState() == Qt.Checked

        elif self.kind == Item.KIND_INT:
            return int(self.body.text())

        elif self.kind == Item.KIND_FLOAT:
            return float(self.body.text())

        elif self.kind == Item.KIND_STRING:
            return self.body.text()

        elif self.kind == Item.KIND_COLOR:
            # FIXME: not implemented
            return Color()

        else:
            assert False, "unknown item type: %r" % self.kind


class PropertiesWidget(QWidget):
    """
    A widget for displaying & editing properties of objects etc.

    Also see the properties this likes to display:
    also see: supertux/property.py
    """

    def __init__(self, parent):
        super().__init__(parent)
        self.items = []

        self.vbox = QVBoxLayout()
        self.layout = QFormLayout()
        self.vbox.addLayout(self.layout)

        self.setLayout(self.vbox)
        self.setMinimumWidth(300)

        # Called in many cases. This should have functions connected
        # which cause the changes in the widget to be applied
        # Called by hitting "Apply", "Ok" or "Finish"
        self.call_signal = Signal()

        self.call_signal.connect(self.call_callbacks)

    def clear_properties(self):
        # Clear Items
        self.items = []
        # Remove all widgets
        for i in range(self.layout.count()):
            self.layout.layout().takeAt(0).widget().setParent(None)

    def set_properties(self, props):
        # Clear previous properties
        self.clear_properties()

        # Add all properties
        for prop in props:
            prop.property_dialog(self)

    # See generic_dialog.py for more about these:

    def add_label(self, text):
        label = QLabel(text)
        self.layout.addRow(label)
        self.items.append(Item(Item.KIND_LABEL, label, None, None))

    def add_bool(self, name, value, callback):
        # FIXME: Qt.Checked doesn't exist!
        def state_change(self, state):
            if callback:
                callback(state == Qt.QChecked)

        label = QLabel(name)
        checkbox = QCheckBox()
        checkbox.stateChanged.connect(state_change)
        self.layout.addRow(label, checkbox)

        if value:
            checkbox.setCheckState(Qt.Checked)

        self.items.append(Item(Item.KIND_BOOL, label, checkbox, callback=callback))

    def add_int(self, name, value, callback=None):
        def text_change(text):
            if callback:
                callback(int(text))

        label = QLabel(name)
        inputbox = QSpinBox()
        inputbox.valueChanged.connect(text_change)
        self.layout.addRow(label, inputbox)

        inputbox.setValue(value)

        self.items.append(Item(Item.KIND_INT, label, inputbox, callback=callback))

    def add_float(self, name, value, callback=None):
        def text_change(text):
            if callback:
                callback(float(text))

        label = QLabel(name)
        inputbox = QLineEdit()
        inputbox.textChanged.connect(text_change)
        self.layout.addRow(label, inputbox)
        inputbox.setText(str(value))

        self.items.append(Item(Item.KIND_FLOAT, label, inputbox, callback=callback))

    def add_string(self, name, value, callback=None, placeholder=None):
        label = QLabel(name)
        inputbox = QLineEdit()
        if callback:
            inputbox.textChanged.connect(callback)
        self.layout.addRow(label, inputbox)

        inputbox.setText(value)
        if placeholder != None:
            inputbox.setPlaceholderText(placeholder)

        self.items.append(Item(Item.KIND_STRING, label, inputbox, callback=callback))

    def add_file(self, label, default, ret_rel_to=None, show_rel_to=None, open_in=None, filters=("All Files (*)",),
                 callback=None):
        """Open a FileDialog for the user to select a file

        :param ret_rel_to: Path to which the param. of callback(value)
         will be relative to
        :param show_rel_to: Path to which the displayed text (in input box)
         will be relative to
        :param open_in: Where the open file dialog will begin
        :param callback: Method/function to call upon file being chosen.
        :param filters: A tuple containing filters for filenames. They should appear like this:
                        Name of Filter (*.txt)
                        ^ Only show .txt files
                        All Files (*)
                        C++ Files (*.cpp *.h *.hpp *.cxx)
        """
        label = QLabel(label)
        inputbox = QLineEdit(default)
        if callback:
            inputbox.textChanged.connect(callback)
        browse = QPushButton("Browse...")

        def file_selected(path):
            """Called whenever file is selected"""
            actual_path = path
            if show_rel_to and path[:len(show_rel_to)] == show_rel_to:
                inputbox.setText(path[len(show_rel_to):])
            else:
                inputbox.setText(path)

            if callback:
                if ret_rel_to and path[:len(ret_rel_to)] == ret_rel_to:
                    callback(path[len(ret_rel_to):])
                else:
                    callback(path)

        def browse_files():
            """Called when Browse... button clicked"""
            dialog = OpenFileDialog("Open File")
            dialog.set_directory(open_in if open_in else Config.current.datadir)
            dialog.run(file_selected)

        browse.clicked.connect(browse_files)  # Connect the above to click signal

        self.layout.addRow(label, inputbox)
        self.layout.addRow(browse)

        self.items.append(Item(Item.KIND_STRING, label, inputbox, callback=callback))

    def add_enum(self, name, values, current_value=0, callback=None):
        label = QLabel(name)
        drop_down = QComboBox()
        for value in values:
            drop_down.addItem(value)
        drop_down.setCurrentIndex(current_value)
        self.layout.addRow(label, drop_down)

        def button_clicked(i):
            if callback:
                callback(values[i])

        drop_down.currentIndexChanged.connect(button_clicked)
        self.items.append(Item(Item.KIND_ENUM, label, drop_down, callback=callback, group=None))

    def add_radio(self, name, values, current_value=0, callback=None):
        label = QLabel(name)
        group = QButtonGroup()
        for i, value in enumerate(values):
            radio = QRadioButton(value)
            radio.setChecked(current_value == i)
            if i == 0:
                self.layout.addRow(label, radio)
            else:
                self.layout.addRow(None, radio)
            group.addButton(radio)

        def button_clicked(button):
            if callback:
                callback(button.text())

        group.buttonClicked.connect(button_clicked)
        self.items.append(Item(Item.KIND_ENUM, label, None, callback=callback, group=group))

    def add_color(self, name, color, callback=None):
        """Not fully implemented according to Item class at the top."""
        label = QLabel(name)
        pixmap = QPixmap(32, 32)
        pixmap.fill(color.to_qt())
        icon = QIcon(pixmap)
        colorbutton = QPushButton(icon, color.to_hex())

        def on_color(qcolor):
            pixmap.fill(qcolor)
            icon.addPixmap(pixmap)
            colorbutton.setIcon(icon)
            colorbutton.setText(qcolor.name())

            if callback:
                callback(Color(qcolor.r, qcolor.g, qcolor.b))

        def on_click():
            color_dialog = QColorDialog(self.dialog)
            color_dialog.colorSelected.connect(on_color)
            color_dialog.show()

        colorbutton.clicked.connect(on_click)

        self.layout.addRow(label, colorbutton)

        self.items.append(Item(Item.KIND_COLOR, label, colorbutton, callback=callback))

    def call_callbacks(self, *args):
        for item in self.items:
            if item.callback is not None:
                item.callback(item.get_value())

    def get_values(self):
        result = []

        for item in self.items:
            value = item.get_value()
            if value is not None:
                result.append(value)

        return result

    def add_callback(self, callback):
        """Adds a callback to the callback signal"""
        self.call_signal.connect(callback)

    def call(self):
        self.call_signal(*self.get_values())
