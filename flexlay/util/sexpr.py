#!/usr/bin/env python

# Copyright (c) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This software is provided 'as-is', without any express or implied
# warranty. In no event will the authors be held liable for any damages
# arising from the use of this software.
#
# Permission is granted to anyone to use this software for any purpose,
# including commercial applications, and to alter it and redistribute it
# freely, subject to the following restrictions:
#
# 1. The origin of this software must not be misrepresented; you must not
#    claim that you wrote the original software. If you use this software
#    in a product, an acknowledgment in the product documentation would be
#    appreciated but is not required.
# 2. Altered source versions must be plainly marked as such, and must not be
#    misrepresented as being the original software.
# 3. This notice may not be removed or altered from any source distribution.


import re
import codecs


def sexpr_read_from_file(filename):
    with open(filename, "rt") as fin:
        content = fin.read()
        return parse(content, filename)

def parse(string, context=None):
    parser = SExprParser(string)
    try:
        return parser.parse()
    except Exception as e:
        raise SExprParseError(context, parser.line, parser.column, str(e))


def num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)


class SExprParseError(Exception):

    def __init__(self, context, line, column, message):
        super().__init__("%s:%d:%d: error: %s" % (context, line, column, message))
        self.context = context
        self.line = line
        self.column = column


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
            self.atom += self.text[i]
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


# EOF #
