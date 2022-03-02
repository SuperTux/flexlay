#!/usr/bin/python3

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

import sys

from PyQt5.QtWidgets import (QMainWindow, QApplication, QVBoxLayout,
                         QWidget)
from PyQt5.QtCore import Qt

from supertux.text_editor.supertux_text_edit import SupertuxTextEdit
from supertux.text_editor.highlighters.lisp_highlighter import SuperTuxLispHighlighter


class SupertuxTextEditMainWindow(QMainWindow):

    def __init__(self, text_file=None):
        super().__init__()

        self.setWindowTitle("Supertux Text Editor")

        self.central_widget = QWidget()
        self.setCentralWidget(self.central_widget)
        layout = QVBoxLayout()
        self.ide_widget = SupertuxTextEdit(self)
        layout.addWidget(self.ide_widget)
        self.central_widget.setLayout(layout)

        highlighter = SuperTuxLispHighlighter(self.ide_widget, text_file)
        highlighter.rehighlight()


def main():
    sys.path.append(".")

    #file_path = input("Type the path to the file you'd like to open:")
    text_file = open("example.stl") #open(file_path, "r")

    app = QApplication(sys.argv)

    window = SupertuxTextEditMainWindow(text_file)
    window.show()

    app.exec_()

    #text_file.close()


# EOF #
