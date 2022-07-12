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


from typing import Any, Callable, Optional

from PyQt5.QtCore import Qt
from PyQt5.QtGui import QIcon, QPixmap, QColor
from PyQt5.QtWidgets import (
    QButtonGroup,
    QCheckBox,
    QComboBox,
    QFormLayout,
    QLabel,
    QLineEdit,
    QPushButton,
    QRadioButton,
    QSpinBox,
    QVBoxLayout,
    QWidget,
)

from flexlay.color import Color
from flexlay.gui import OpenFileDialog
from flexlay.util.config import Config
from flexlay.util.signal import Signal
from flexlay.property import Property


class Item:

    KIND_LABEL = 0
    KIND_BOOL = 1
    KIND_INT = 2
    KIND_FLOAT = 3
    KIND_STRING = 4
    KIND_ENUM = 5
    KIND_RADIO = 6
    KIND_COLOR = 7

    def __init__(self, kind: int,
                 label: QLabel,
                 body: Any,  # FIXME: turn this whole class into proper OOP
                 callback: Optional[Callable[[Any], None]] = None,
                 group: Optional[QButtonGroup] = None) -> None:
        self.kind: int = kind
        self.label: QLabel = label
        self.body: Optional[Any] = body
        self.group: Optional[Any] = group
        self.callback: Optional[Callable[[Any], None]] = callback

    def get_value(self) -> Any:
        if self.kind == Item.KIND_LABEL:
            return None

        elif self.kind == Item.KIND_RADIO:
            idx = 0
            assert self.group is not None
            for button in self.group.buttons():
                if button == self.group.checkedButton():
                    return idx
                idx += 1

        elif self.kind == Item.KIND_ENUM:
            assert self.body is not None
            return self.body.currentText()

        elif self.kind == Item.KIND_BOOL:
            assert self.body is not None
            return self.body.checkState() == Qt.Checked

        elif self.kind == Item.KIND_INT:
            assert self.body is not None
            return int(self.body.text())

        elif self.kind == Item.KIND_FLOAT:
            assert self.body is not None
            return float(self.body.text())

        elif self.kind == Item.KIND_STRING:
            assert self.body is not None
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

    def __init__(self, parent: QWidget) -> None:
        super().__init__(parent)
        self.items: list[Item] = []

        self.vbox = QVBoxLayout()
        self._layout = QFormLayout()
        self.vbox.addLayout(self._layout)

        self.setLayout(self.vbox)
        self.setMinimumWidth(300)

        # Called in many cases. This should have functions connected
        # which cause the changes in the widget to be applied
        # Called by hitting "Apply", "Ok" or "Finish"
        self.call_signal = Signal()

        self.call_signal.connect(self.call_callbacks)

    def clear_properties(self) -> None:
        # Clear Items
        self.items = []
        # Remove all widgets
        for i in range(self._layout.count()):
            # self._layout.layout().takeAt(0).widget().setParent(None)
            self._layout.removeWidget(self._layout.layout().takeAt(0).widget())

    def set_properties(self, props: list[Property]) -> None:
        # Clear previous properties
        self.clear_properties()

        # Add all properties
        for prop in props:
            prop.property_dialog(self)

    def add_label(self, text: str) -> None:
        label = QLabel(text)
        self._layout.addRow(label)
        self.items.append(Item(Item.KIND_LABEL, label, None, None))

    def add_bool(self, name: str, value: bool, callback: Callable[[bool], None]) -> None:
        label = QLabel(name)
        checkbox = QCheckBox()
        self._layout.addRow(label, checkbox)

        if value:
            checkbox.setCheckState(Qt.Checked)

        self.items.append(Item(Item.KIND_BOOL, label, checkbox, callback=callback))

    def add_int(self, name: str, value: int, callback: Optional[Callable[[int], None]] = None) -> None:
        label = QLabel(name)
        inputbox = QSpinBox()
        self._layout.addRow(label, inputbox)

        inputbox.setValue(value)

        self.items.append(Item(Item.KIND_INT, label, inputbox, callback=callback))

    def add_float(self, name: str, value: float, callback: Optional[Callable[[float], None]] = None) -> None:
        label = QLabel(name)
        inputbox = QLineEdit()
        self._layout.addRow(label, inputbox)
        inputbox.setText(str(value))

        self.items.append(Item(Item.KIND_FLOAT, label, inputbox, callback=callback))

    def add_string(self, name: str, value: str,
                   callback: Optional[Callable[[str], None]] = None,
                   placeholder: Optional[str] = None) -> None:
        label = QLabel(name)
        inputbox = QLineEdit()
        self._layout.addRow(label, inputbox)

        inputbox.setText(value)
        if placeholder is not None:
            inputbox.setPlaceholderText(placeholder)

        self.items.append(Item(Item.KIND_STRING, label, inputbox, callback=callback))

    def add_file(self, label: str, default: str,
                 ret_rel_to: Optional[str] = None,
                 show_rel_to: Optional[str] = None,
                 open_in: Optional[str] = None,
                 filters: tuple[str] = ("All Files (*)",),
                 callback: Optional[Callable[[Any], None]] = None) -> None:
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
        browse = QPushButton("Browse...")

        def file_selected(path: str) -> None:
            """Called whenever file is selected"""
            # actual_path = path
            if show_rel_to and path[:len(show_rel_to)] == show_rel_to:
                inputbox.setText(path[len(show_rel_to):])
            else:
                inputbox.setText(path)

        def browse_files() -> None:
            """Called when Browse... button clicked"""
            dialog = OpenFileDialog("Open File")
            assert Config.current is not None
            dialog.set_directory(open_in if open_in else Config.current.datadir)
            dialog.run(file_selected)

        browse.clicked.connect(browse_files)  # Connect the above to click signal

        self._layout.addRow(label, inputbox)
        self._layout.addRow(browse)

        self.items.append(Item(Item.KIND_STRING, label, inputbox, callback=callback))

    def add_enum(self, name: str, values: list[str],
                 current_value: int = 0,
                 callback: Optional[Callable[[int], None]] = None) -> None:
        label = QLabel(name)
        drop_down = QComboBox()
        for value in values:
            drop_down.addItem(value)
        drop_down.setCurrentIndex(current_value)
        self._layout.addRow(label, drop_down)

        self.items.append(Item(Item.KIND_ENUM, label, drop_down, callback=callback, group=None))

    def add_radio(self, name: str, values: list[str],
                  current_value: int = 0,
                  callback: Optional[Callable[[int], None]] = None) -> None:
        label = QLabel(name)
        group = QButtonGroup()
        for i, value in enumerate(values):
            radio = QRadioButton(value)
            radio.setChecked(current_value == i)
            if i == 0:
                self._layout.addRow(label, radio)
            else:
                self._layout.addRow(radio)
            group.addButton(radio)

        self.items.append(Item(Item.KIND_ENUM, label, None, callback=callback, group=group))

    def add_color(self, name: str, color: Color, callback: Callable[[str], None] = None) -> None:
        """Not fully implemented according to Item class at the top."""
        label = QLabel(name)
        pixmap = QPixmap(32, 32)
        pixmap.fill(color.to_qt())
        icon = QIcon(pixmap)
        colorbutton = QPushButton(icon, color.to_hex())

        def on_color(qcolor: QColor) -> None:
            pixmap.fill(qcolor)
            icon.addPixmap(pixmap)
            colorbutton.setIcon(icon)
            colorbutton.setText(qcolor.name())

        self._layout.addRow(label, colorbutton)

        self.items.append(Item(Item.KIND_COLOR, label, colorbutton, callback=callback))

    def call_callbacks(self, *args: Any) -> None:
        for item in self.items:
            if item.callback is not None:
                item.callback(item.get_value())

    def get_values(self) -> list[Any]:
        result = []

        for item in self.items:
            value = item.get_value()
            if value is not None:
                result.append(value)

        return result

    def add_callback(self, callback: Callable[[Any], None]) -> None:
        """Adds a callback to the callback signal"""
        self.call_signal.connect(callback)

    def call(self) -> None:
        self.call_signal(*self.get_values())


# EOF #
