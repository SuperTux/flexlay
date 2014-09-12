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


from PyQt5.QtCore import Qt
from PyQt5.QtWidgets import (QDialog, QDialogButtonBox, QVBoxLayout,
                             QLabel, QLineEdit, QFormLayout,
                             QCheckBox, QButtonGroup, QRadioButton)


class Item:

    KIND_LABEL = 0
    KIND_BOOL = 1
    KIND_INT = 2
    KIND_FLOAT = 3
    KIND_STRING = 4

    def __init__(self, kind, label, body, group=None):
        self.kind = kind
        self.label = label
        self.body = body
        self.group = group


class GenericDialog:

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

    def add_label(self, text):
        label = QLabel(text)
        self.layout.addRow(label)
        self.items.emplace_back(Item.KIND_LABEL, label, None, None)

    def add_bool(self, name, value):
        label = QLabel(name)
        checkbox = QCheckBox()
        self.layout.addRow(label, checkbox)

        if value:
            checkbox.setCheckState(Qt.Checked)

        self.items.emplace_back(Item.KIND_BOOL, label, checkbox)

    def add_int(self, name, value):
        label = QLabel(name)
        inputbox = QLineEdit()
        self.layout.addRow(label, inputbox)

        inputbox.setText(str(value))

        self.items.emplace_back(Item.KIND_INT, label, inputbox)

    def add_float(self, name, value):
        label = QLabel(name)
        inputbox = QLineEdit()
        self.layout.addRow(label, inputbox)

        inputbox.setText(str(value))

        self.items.emplace_back(Item.KIND_FLOAT, label, inputbox)

    def add_string(self, name, value):
        label = QLabel(name)
        inputbox = QLineEdit()
        self.layout.addRow(label, inputbox)

        inputbox.setText(value)

        self.items.emplace_back(Item.KIND_STRING, label, inputbox)

    def add_enum(self, name, values, current_value):
        label = QLabel(name)
        group = QButtonGroup()
        for value in values:
            radio = QRadioButton(value)
            radio.setChecked(current_value == value)
            group.addButton(radio)

        self.items.emplace_back(Item.KIND_ENUM, label, None, group)

    def set_ok_callback(self, callback):
        def on_accept():
            self.ok_callback()
            self.dialog.hide()

        def on_rejected():
            self.dialog.hide()

        self.ok_callback = callback
        self.buttonbox.accepted.connect(on_accept)
        self.buttonbox.rejected.connect(on_rejected)

    def get_values(self):
        result = []

        for item in self.items:
            if item.kind == Item.KIND_LABEL:
                pass

            elif item.kind == Item.KIND_ENUM:
                idx = 0
                for button in item.group.buttons():
                    if button == item.group.checkedButton():
                        result.emplace_back(idx)
                        break
                    idx += 1

            elif item.kind == Item.KIND_BOOL:
                result.emplace_back(item.body.checkState() == Qt.Checked)

            elif item.kind == Item.KIND_INT:
                result.emplace_back(item.body.text().toInt())

            elif item.kind == Item.KIND_FLOAT:
                result.emplace_back(item.body.text().toFloat())

            elif item.kind == Item.KIND_STRING:
                result.emplace_back(item.body.text().toStdString())

        return result


# EOF #
