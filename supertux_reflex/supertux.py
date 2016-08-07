# Flexlay - A Generic 2D Game Editor
#
# ISC License
# Copyright (C) 2016 Karkus476 <karkus476@yahoo.com>
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR ON SEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.

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
    file_menu.add_callback("Save", save_clicked)
    file_menu.add_callback("Open", open_clicked)

    editor.run()