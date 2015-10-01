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

from PyQt4.QtGui import (QDialog, QDialogButtonBox, QVBoxLayout,
                         QLabel, QLineEdit, QFormLayout, QPushButton,
                         QIcon, QCheckBox, QPixmap, QButtonGroup,
                         QRadioButton, QColorDialog, QWidget, QFileDialog)
from .property import FileProperty
from flexlay.gui.generic_dialog import Item

class PropertiesWidget(QWidget):
    '''
    Uses a list of [supertux/property]s to display a properties dialog with boxes
    
    A class which can display properties in a docked widget form.
    If you add capabilities here, you should be able to copy-paste them into:
    @see: flexlay/gui/generic_dialog.py
    Also see the properties this displays:
    @see: supertux/property.py
    '''
    
    def __init__(self, parent):
        super().__init__(parent)
        self.items = []
        self.ok_callback = None

        self.vbox = QVBoxLayout()
        self.layout = QFormLayout()
        self.vbox.addLayout(self.layout)

        self.setLayout(self.vbox)
        self.setMinimumWidth(300)
        self.show()
        
    def set_properties(self, props):
        #Clear Items
        self.items = []
        #Remove all widgets
        for i in range(self.layout.count()):
            self.layout.layout().takeAt(0).widget().setParent(None)
        #Add all properties
        for prop in props:
            prop.property_dialog(self)
    
    #See generic_dialog.py for more about these:
    
    def add_label(self, text): #If changed, update in flexlay/gui/generic_dialog.py
        label = QLabel(text)
        self.layout.addRow(label)
        self.items.append(Item(Item.KIND_LABEL, label, None, None))

    def add_bool(self, name, value, callback): #If changed, update in flexlay/gui/generic_dialog.py
        def state_change(self, state):
            callback(state == Qt.QChecked)
        label = QLabel(name)
        checkbox = QCheckBox()
        checkbox.stateChanged.connect(state_change)
        self.layout.addRow(label, checkbox)

        if value:
            checkbox.setCheckState(Qt.Checked)

        self.items.append(Item(Item.KIND_BOOL, label, checkbox, callback=callback))

    def add_int(self, name, value, callback=None): #If changed, update in flexlay/gui/generic_dialog.py
        def text_change(text):
            callback(int(text))
        label = QLabel(name)
        inputbox = QLineEdit()
        inputbox.textChanged.connect(text_change)
        self.layout.addRow(label, inputbox)

        inputbox.setText(str(value))

        self.items.append(Item(Item.KIND_INT, label, inputbox, callback=callback))

    def add_float(self, name, value, callback=None): #If changed, update in flexlay/gui/generic_dialog.py
        def text_change(text):
            callback(float(text))
        label = QLabel(name)
        inputbox = QLineEdit()
        inputbox.textChanged.connect(text_change)
        self.layout.addRow(label, inputbox)

        inputbox.setText(str(value))

        self.items.append(Item(Item.KIND_FLOAT, label, inputbox, callback=callback))
        
    def add_string(self, name, value, callback=None): #If changed, update in flexlay/gui/generic_dialog.py
        label = QLabel(name)
        inputbox = QLineEdit()
        inputbox.textChanged.connect(callback)
        self.layout.addRow(label, inputbox)

        inputbox.setText(value)

        self.items.append(Item(Item.KIND_STRING, label, inputbox, callback=callback))
        
    def add_file(self, label, default, relative_to=None, open_in=None, callback=None):
        label = QLabel(file_property.label)
        inputbox = QLineEdit(file_property.default)
        inputbox.textChanged.connect(callback)
        browse = QPushButton("Browse...")
        
        def browse_files():
            '''Called when Browse... button clicked'''
            path = QFileDialog.getOpenFileName(None, "Open File", 
                                              open_in)
            file_property.actual_path = path
            if path[:len(file_property.relative_to)] == relative_to:
                inputbox.setText(path[len(relative_to):])
            else:
                inputbox.setText(path)
                
        browse.clicked.connect(browse_files) #Connect the above to click signal
        self.layout.addRow(label, inputbox)
        self.layout.addRow(browse)

        self.items.append(Item(Item.KIND_STRING, label, inputbox, callback=callback))

    def add_enum(self, name, values, current_value=0, callback=None): #If changed, update in flexlay/gui/generic_dialog.py
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
            callback(button.text())
        group.buttonClicked.connect(button_clicked)
        self.items.append(Item(Item.KIND_ENUM, label, None, callback=callback, group=group))

    def add_color(self, name, color, callback=None): #If changed, update in flexlay/gui/generic_dialog.py
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

    def set_callback(self, callback): #If changed, update in flexlay/gui/generic_dialog.py
        def on_accept():
            self.ok_callback(*self.get_values())
            self.dialog.hide()

        def on_rejected():
            self.dialog.hide()

        self.ok_callback = callback
        self.buttonbox.accepted.connect(on_accept)
        self.buttonbox.rejected.connect(on_rejected)

    def call_callbacks(self): #If changed, update in flexlay/gui/generic_dialog.py
        for item in self.items:
            if item.callback is not None:
                item.callback(item.get_value())

    def get_values(self): #If changed, update in flexlay/gui/generic_dialog.py
        result = []

        for item in self.items:
            value = item.get_value()
            if value is not None:
                result.append(value)

        return result
