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


from PyQt4.QtCore import Qt
from PyQt4.QtGui import (QDialog, QDialogButtonBox, QVBoxLayout,
                         QLabel, QLineEdit, QFormLayout, QPushButton,
                         QIcon, QCheckBox, QPixmap, QButtonGroup,
                         QRadioButton, QColorDialog)

from ..color import Color


class Item:

    KIND_LABEL = 0
    KIND_BOOL = 1
    KIND_INT = 2
    KIND_FLOAT = 3
    KIND_STRING = 4
    KIND_ENUM = 5
    KIND_COLOR = 6

    def __init__(self, kind, label, body, callback=None, group=None):
        self.kind = kind
        self.label = label
        self.body = body
        self.group = group
        self.callback = None

    def get_value(self):
        if self.kind == Item.KIND_LABEL:
            return None

        elif self.kind == Item.KIND_ENUM:
            idx = 0
            for button in self.group.buttons():
                if button == self.group.checkedButton():
                    return idx
                idx += 1

        elif self.kind == Item.KIND_BOOL:
            return (self.body.checkState() == Qt.Checked)

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


class GenericDialog:
    '''
    A class which can display properties in a dialog.
    If you add capabilities here, you should be able to
    copy-paste them into:
    @see: supertux/properties_widget.py
    Also see the properties this displays:
    @see: supertux/property.py
    '''
    def __init__(self, title, parent):
        self.items = []
        self.ok_callback = None

        self.dialog = QDialog(parent)

        self.dialog.setModal(True)
        self.dialog.setWindowTitle(title)
        self.buttonbox = QDialogButtonBox(QDialogButtonBox.Ok | QDialogButtonBox.Cancel)

        vbox = QVBoxLayout()
        self.layout = QFormLayout()
        vbox.addLayout(self.layout)
        vbox.addWidget(self.buttonbox)

        self.dialog.setLayout(vbox)
        self.dialog.setMinimumWidth(300)
        self.dialog.show()

    #Also found in properties_widget.py

    def add_label(self, text):
        label = QLabel(text)
        self.layout.addRow(label)
        self.items.append(Item(Item.KIND_LABEL, label, None, None))

    def add_bool(self, name, value, callback):
        def state_change(self, state):
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
            callback(int(text))
        label = QLabel(name)
        inputbox = QLineEdit()
        inputbox.textChanged.connect(text_change)
        self.layout.addRow(label, inputbox)

        inputbox.setText(str(value))

        self.items.append(Item(Item.KIND_INT, label, inputbox, callback=callback))

    def add_float(self, name, value, callback=None):
        def text_change(text):
            callback(float(text))
        label = QLabel(name)
        inputbox = QLineEdit()
        inputbox.textChanged.connect(text_change)
        self.layout.addRow(label, inputbox)

        inputbox.setText(str(value))

        self.items.append(Item(Item.KIND_FLOAT, label, inputbox, callback=callback))

    def add_string(self, name, value, callback=None):
        label = QLabel(name)
        inputbox = QLineEdit()
        inputbox.textChanged.connect(callback)
        self.layout.addRow(label, inputbox)

        inputbox.setText(value)

        self.items.append(Item(Item.KIND_STRING, label, inputbox, callback=callback))

    #Untested in this context, but works in properties widget
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
        def button_clicked(button):
            callback(button.text())
        group.buttonClicked.connect(button_clicked)
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
    


# EOF #
