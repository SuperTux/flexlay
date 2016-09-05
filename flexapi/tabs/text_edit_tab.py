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

from PyQt4.QtGui import QTextEdit

from flexapi.resources import TextFileResource
from .editor_tab import EditorTab


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
