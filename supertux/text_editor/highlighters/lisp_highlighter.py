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


from typing import Any, IO, Optional

import json
import re

from PyQt5.QtGui import (QTextCharFormat, QFont)
from PyQt5.QtCore import Qt
from PyQt5.QtWidget import QTextEdit

from .highlighter import SuperTuxHighlighter, HighlightingRule


class SuperTuxLispHighlighter(SuperTuxHighlighter):

    @staticmethod
    def clean_text(text: str) -> str:
        tiles = text.find("(tiles")
        while tiles != -1:
            close_bracket = text.find(")", tiles)
            assert close_bracket >= 0
            before_length = len(text)
            text = text[:tiles] + "(tiles" + text[close_bracket:]
            after_length = len(text)
            delta_length = before_length - after_length
            tiles = text.find("(tiles", close_bracket - delta_length)
        return text

    @staticmethod
    def load_tree_json(filename: Optional[str] = None) -> Any:
        if not filename or filename[-5:] != ".json":
            filename = "highlighters/patterns2.json"
        patterns_file = open(filename, "r")
        pattern_tree = json.loads(patterns_file.read())
        return pattern_tree

    @staticmethod
    def search_tree(tree: Any, tag_list: list[str]) -> Optional[QTextCharFormat]:
        '''
        Searches a tree to find a tag
        :param tag_list: ["supertux-level", "sector", "name"]
        :return: QTextCharFormat if possible, else None
        '''
        if tag_list[0] != "supertux-level":
            print("lisp_highlighter.py Line 53, tag_list is not from a supertux-level")
            return None
        tree = tree[tag_list.pop(0)]
        try:
            while len(tag_list) > 0:
                tree = tree["branches"][tag_list.pop(0)]
        except KeyError:
            return None

        colour = tree["color"]

        format = QTextCharFormat()
        if colour == "black":
            format.setForeground(Qt.black)
        elif colour == "blue":
            format.setForeground(Qt.blue)
        elif colour == "red":
            format.setForeground(Qt.red)
        elif colour == "green":
            format.setForeground(Qt.green)
        elif colour == "darkGreen":
            format.setForeground(Qt.darkGreen)
        elif colour == "darkBlue":
            format.setForeground(Qt.darkBlue)
        elif colour == "darkRed":
            format.setForeground(Qt.darkRed)
        elif colour == "magenta":
            format.setForeground(Qt.magenta)

        if tree["bold"]:
            format.setFontWeight(QFont.Bold)
        if tree["italic"]:
            format.setFontItalic(True)

        return format

    def __init__(self, text_edit: QTextEdit, level_file: IO[str]):
        super().__init__(text_edit)

        text = level_file.read()
        text = SuperTuxLispHighlighter.clean_text(text)
        text_edit.setText(text)

        self.highlighting_rules += SuperTuxHighlighter.load_patterns("highlighters/patterns.json")

        string_format = QTextCharFormat()
        string_format.setForeground(Qt.darkRed)
        string_pattern = '"'
        self.string = HighlightingRule(string_pattern, string_format, "string")

        # comment_format = QTextCharFormat()
        # comment_format.setForeground(Qt.darkRed)
        # comment_pattern = r';.*'
        # comment = HighlightingRule(comment_pattern, comment_format)
        #
        # self.highlighting_rules.append(comment)

        # tree_json = SuperTuxLispHighlighter.load_tree_json()
        # SuperTuxLispHighlighter.search_tree(tree_json,["supertux-level", "sector", "name"])

    def highlightBlock(self, text: str) -> None:
        for rule in self.highlighting_rules:
            search = re.search(rule.pattern, text)
            span = None if not search else search.span()
            while span:
                length = span[1] - span[0]
                self.setFormat(span[0], length, rule.format)
                search = re.search(rule.pattern, text[span[1]:])
                span = None if not search else search.span()
        self.setCurrentBlockState(0)


# EOF #
