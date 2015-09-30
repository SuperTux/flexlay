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
# You should have received a copy ofQWidget the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

from PyQt4.QtGui import (QDialog, QDialogButtonBox, QVBoxLayout,
                         QLabel, QLineEdit, QFormLayout, QPushButton,
                         QIcon, QCheckBox, QPixmap, QButtonGroup,
                         QRadioButton, QColorDialog, QWidget)
from .property import StringProperty
from flexlay.gui.generic_dialog import Item

class PropertiesWidget(QWidget):
    '''Mostly based around Grumbel's flexlay/gui/generic_dialog.py - GenericDialog
    Uses a list of [supertux/property]s to display a properties dialog with boxes'''
    def __init__(self, parent):
        super().__init__(parent)
        self.items = []
        self.ok_callback = None

        prop_test = [StringProperty("label", "identifier", default="default"), StringProperty("test", "identifier", default="default")]

        vbox = QVBoxLayout()
        self.layout = QFormLayout()
        vbox.addLayout(self.layout)

        self.setLayout(vbox)
        self.setMinimumWidth(300)
        self.show()
        
        self.set_properties(prop_test)

    @staticmethod
    def from_properties(self, props, parent):
        widget = PropertiesWidget(parent)
        for prop in props:
            prop.property_dialog(widget)
        return widget
        
    def set_properties(self, props):
        for prop in props:
            prop.property_dialog(self)
    
    def add_label(self, text):
        label = QLabel(text)
        self.layout.addRow(label)
        self.items.append(Item(Item.KIND_LABEL, label, None, None))

    def add_bool(self, name, value, callback):
        label = QLabel(name)
        checkbox = QCheckBox()
        self.layout.addRow(label, checkbox)

        if value:
            checkbox.setCheckState(Qt.Checked)

        self.items.append(Item(Item.KIND_BOOL, label, checkbox, callback=callback))

    def add_int(self, name, value, callback=None):
        label = QLabel(name)
        inputbox = QLineEdit()
        self.layout.addRow(label, inputbox)

        inputbox.setText(str(value))

        self.items.append(Item(Item.KIND_INT, label, inputbox, callback=callback))

    def add_float(self, name, value, callback=None):
        label = QLabel(name)
        inputbox = QLineEdit()
        self.layout.addRow(label, inputbox)

        inputbox.setText(str(value))

        self.items.append(Item(Item.KIND_FLOAT, label, inputbox, callback=callback))

    def add_string(self, name, value, callback=None):
        label = QLabel(name)
        inputbox = QLineEdit()
        self.layout.addRow(label, inputbox)

        inputbox.setText(value)

        self.items.append(Item(Item.KIND_STRING, label, inputbox, callback=callback))

    def add_enum(self, name, values, current_value=0, callback=None):
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
        self.items.append(Item(Item.KIND_ENUM, label, None, callback=callback, group=group))

    def add_color(self, name, color, callback=None):
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

    def set_callback(self, callback):
        def on_accept():
            self.ok_callback(*self.get_values())
            self.dialog.hide()

        def on_rejected():
            self.dialog.hide()

        self.ok_callback = callback
        self.buttonbox.accepted.connect(on_accept)
        self.buttonbox.rejected.connect(on_rejected)

    def call_callbacks(self):
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
