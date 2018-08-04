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

import json, re

from PyQt4.QtGui import QSyntaxHighlighter, QTextCharFormat, QFont
from PyQt4.QtCore import Qt


class SuperTuxHighlighter(QSyntaxHighlighter):

    @staticmethod
    def load_patterns(filename=None, root="supertux-level"):
        if not filename or filename[-5:] != ".json":
            filename = "highlighters/patterns.json"
        rules = []
        patterns_file = open(filename, "r")
        pattern_list = json.loads(patterns_file.read())[root]
        for pattern_json in pattern_list:
            pattern = pattern_json["find"]
            colour = pattern_json["color"]
            id = pattern_json["id"]

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

            if pattern_json["bold"]:
                format.setFontWeight(QFont.Bold)
            if pattern_json["italic"]:
                format.setFontItalic(True)

            rule = HighlightingRule(pattern, format, id=id)
            rules.append(rule)
        print("Done:", len(rules))
        return rules

    def __init__(self, text_edit):
        super().__init__(text_edit)
        self.highlighting_rules = []

    def highlightBlock(self, text):
        for rule in self.highlighting_rules:
            search = re.search(rule.pattern, text)
            span = None if not search else search.span()
            while span:
                length = span[1] - span[0]
                self.setFormat(span[0], length, rule.format)
                search = re.search(rule.pattern, text[span[1]:])
                span = None if not search else search.span()
        self.setCurrentBlockState(0)



class HighlightingRule:
    def __init__(self, pattern, format, id="null"):
        self.pattern = pattern
        self.format = format
        self.id = id