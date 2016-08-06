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

class SExp:
    def __init__(self, name, value):
        self.name = name
        self.value = value

    @staticmethod
    def _from_parsed(parsed):
        """The parsed format is difficult to use,

        so use this method to generate an object from it
        """
        if type(parsed) == list:
            return SExp(parsed[0], SExp._from_parsed())

class SExprParser:
    def __init__(self, text):
        self.text = text

    def state_list(self, c):
        if c is None:
            pass  # handled in parse()
        elif c == '(':
            self.stack.append([])
        elif c == ')':
            self.stack[-2].append(self.stack.pop())
        elif c == "\"":
            self.state = self.state_string
            self.atom = ""
        elif c == ";":
            self.state = self.state_comment
            self.atom = None
        elif c == "#":
            self.state = self.state_bool
            self.atom = "#"
        elif c.isdigit() or c in "-+":
            self.state = self.state_number
            self.atom = c
        elif c.isalpha() or c in "-_":
            self.state = self.state_symbol
            self.atom = c
        elif c.isspace():
            pass
        else:
            raise Exception("unexpected character: '%s'" % c)

    def state_comment(self, c):
        if c == '\n':
            self.state = self.state_list
        else:
            pass

    def state_string(self, c):
        if c is None:
            raise Exception("string not closed at end of file")
        elif c == "\\":
            self.index += 1
            c = self.text[self.index]
            if c == "n":
                self.atom += "\n"
            elif c == "t":
                self.atom += "\t"
            elif c == "\\":
                self.atom += "\\"
            elif c == "\"":
                self.atom += "\""
            else:
                self.atom += "\\"
                self.atom += c
        elif c == "\"":
            self.stack[-1].append(self.atom)
            self.atom = None
            self.state = self.state_list
        else:
            self.atom += c

    def state_bool(self, c):
        if len(self.atom) == 2:
            if self.atom == "#f":
                self.stack[-1].append(False)
            elif self.atom == "#t":
                self.stack[-1].append(True)
            else:
                raise Exception("unknown token: %s" % self.atom)

            self.atom = None
            self.state = self.state_list
            self.index -= 1
        elif c is None:
            raise Exception("incomplete bool: %s" % self.atom)
        else:
            self.atom += c

    def state_number(self, c):
        if c is None or (not c.isdigit() and c != "."):
            self.stack[-1].append(num(self.atom))
            self.atom = None
            self.state = self.state_list
            self.index -= 1
        else:
            self.atom += c

    def state_symbol(self, c):
        if c is None or c.isspace() or c == '(' or c == ')':
            self.stack[-1].append(self.atom)
            self.atom = None
            self.state = self.state_list
            self.index -= 1
        else:
            self.atom += c

    def parse(self):
        self.atom = None
        self.stack = [[]]
        self.state = self.state_list
        self.line = 1
        self.column = 0

        self.index = 0
        while self.index < len(self.text):
            c = self.text[self.index]
            if c == '\n':
                self.line += 1
                self.column = 0
            else:
                self.column += 1
            self.state(c)
            self.index += 1
        self.state(None)

        if len(self.stack) == 1:
            return self.stack[0]
        else:
            raise Exception("list not closed")