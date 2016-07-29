# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2016 Karkus476 <karkus476@yahoo.com>
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

from PyQt4.QtGui import QApplication

from flexapi import FlexlayEditor
from flexapi.elements import EditorElement, MenubarElement
from flexapi.elements.button_panel_element import *
from flexapi.tabs import TextEditTab
from flexapi.resources import TextFileResource


def save_clicked():
    print("This would save a level.")

def open_clicked():
    print("This would open a file.")


def main():
    app = QApplication([])

    editor = FlexlayEditor("supertux_editor", 0)

    editor_map_element = EditorElement()
    editor.add_element("editor_map", editor_map_element)
    editor_map_element.add_tab_type(TextEditTab)
    # editor_map_element.edit("Test", TextFileResource("/home", "user", "testfile.txt"))
    editor_map_element.edit("Test Label", "This is a test!")

    button_panel_element = ButtonPanelElement()
    editor.add_element("button_panel", button_panel_element)
    save_button = SaveButton(save_clicked)
    button_panel_element.add_button("save_button", save_button)
    open_button = OpenButton(open_clicked)
    button_panel_element.add_button("open_button", open_button)

    menubar_element = MenubarElement()
    editor.add_element("menubar", menubar_element)
    file_menu = menubar_element.add_menu("&File")
    file_menu.add_item("Save", save_clicked)
    file_menu.add_item("Open", open_clicked)

    editor.run()