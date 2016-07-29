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

from PyQt4.QtGui import QTextEdit

from .editor_tab import EditorTab
from ..resources.text_file_resource import TextFileResource


class TextEditTab(EditorTab):
    """A simple class to edit values or files in a simple text editor.

    Subclass this to provide further functionality e.g. Syntax Highlighting, or suggestions etc.
    Do not misunderstand this class' purpose: it handles text and sorts out some simple stuff, but is essentially
    abstract
    """
    def __init__(self, item):
        super().__init__(item)
        if type(item) == str:
            self.widget = QTextEdit(item)
        elif isinstance(item, TextFileResource):
            self.widget = QTextEdit(item.read())

    @classmethod
    def can_edit(Tab, item):
        super().can_edit(item)
        if type(item) == str:
            return 2
        elif isinstance(item, TextFileResource):
            return 2

    def get_value(self):
        return self.widget.toPlainText()

    def get_widget(self):
        return self.widget

    def set_text(self, text):
        self.text = text
        self.widget.setText(self.text)