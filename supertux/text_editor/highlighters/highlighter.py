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


from typing import Optional

import json
import re

from PyQt5.QtGui import QSyntaxHighlighter, QTextCharFormat, QFont
from PyQt5.QtWidgets import QTextEdit
from PyQt5.QtCore import Qt


class HighlightingRule:

    def __init__(self, pattern: str, fmt: QTextCharFormat, id: str = "null"):
        self.pattern = pattern
        self.format: QTextCharFormat = fmt
        self.id = id


class SuperTuxHighlighter(QSyntaxHighlighter):

    @staticmethod
    def load_patterns(filename: Optional[str] = None, root: str = "supertux-level") -> list[HighlightingRule]:
        if not filename or filename[-5:] != ".json":
            filename = "highlighters/patterns.json"
        rules = []
        patterns_file = open(filename, "r")
        pattern_list = json.loads(patterns_file.read())[root]
        for pattern_json in pattern_list:
            pattern = pattern_json["find"]
            colour = pattern_json["color"]
            id = pattern_json["id"]

            fmt = QTextCharFormat()
            if colour == "black":
                fmt.setForeground(Qt.black)
            elif colour == "blue":
                fmt.setForeground(Qt.blue)
            elif colour == "red":
                fmt.setForeground(Qt.red)
            elif colour == "green":
                fmt.setForeground(Qt.green)
            elif colour == "darkGreen":
                fmt.setForeground(Qt.darkGreen)
            elif colour == "darkBlue":
                fmt.setForeground(Qt.darkBlue)
            elif colour == "darkRed":
                fmt.setForeground(Qt.darkRed)
            elif colour == "magenta":
                fmt.setForeground(Qt.magenta)

            if pattern_json["bold"]:
                fmt.setFontWeight(QFont.Bold)
            if pattern_json["italic"]:
                fmt.setFontItalic(True)

            rule = HighlightingRule(pattern, fmt, id=id)
            rules.append(rule)
        print("Done:", len(rules))
        return rules

    def __init__(self, text_edit: QTextEdit) -> None:
        super().__init__(text_edit)
        self.highlighting_rules: list[HighlightingRule] = []

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
