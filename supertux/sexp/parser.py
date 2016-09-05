# SExp - A S-Expression Parser for Python
# Copyright (c) 2015 Ingo Ruhnke <grumbel@gmail.com>
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

import sexp


class TextIO:

    def __init__(self, text):
        self.text = text
        self.i = 0

        self.line = 1
        self.column = 0

    def getchar(self):
        c = self.text[self.i]
        if c == '\n':
            self.line += 1
            self.column = 0
        else:
            self.column += 1
        self.i += 1
        return c

    def eof(self):
        return self.i >= len(self.text)

    def ungetchar(self):
        self.i -= 1


class Parser:

    @staticmethod
    def from_file(filename):
        with open(filename, "r") as fin:
            return Parser.from_string(fin.read())

    @staticmethod
    def from_string(text):
        parser = Parser(TextIO(text))
        return parser.parse()

    def __init__(self, io):
        self.io = io
        self.stack = [[]]
        self.state = self.parse_list
        self.atom = ""

    def parse_list(self, c):
        if c == '(':
            self.stack.append([])
        elif c == ')':
            v = self.stack.pop()
            if v == []:
                self.stack[-1].append(sexp.Nil())
            else:
                self.stack[-1].append(sexp.Array(v))
        elif c == "\"":
            self.state = self.parse_string
            self.atom = ""
        elif c == ";":
            self.state = self.parse_comment
        elif c.isalpha():
            self.state = self.parse_symbol
            self.atom = c
        elif c.isdigit():
            self.state = self.parse_number
            self.atom = c
        elif c.isspace():
            pass
        else:
            raise Exception("%d:%d: error: unexpected character: '%s'" %
                            (self.io.line, self.io.column, c))

    def parse_comments(self, c):
        if c == '\n':
            self.state = self.parse_list

    def parse_string(self, c):
        if c == "\\":
            self.atom += self.io.getchar()
        elif c == "\"":
            self.stack[-1].append(sexp.String(self.atom))
            self.state = self.parse_list
        else:
            self.atom += c

    def parse_number(self, c):
        if not c.isdigit() and c != ".":
            if self.atom.count(".") == 0:
                self.stack[-1].append(sexp.Integer(int(self.atom)))
            elif self.atom.count(".") == 1:
                self.stack[-1].append(sexp.Real(float(self.atom)))
            else:
                self.stack[-1].append(sexp.Symbol(self.atom))
            self.state = self.parse_list
            self.io.ungetchar()
        else:
            self.atom += c

    def parse_symbol(self, c):
        if c.isspace() or c == '(' or c == ')':
            self.stack[-1].append(sexp.Symbol(self.atom))
            self.state = self.parse_list
            self.io.ungetchar()
        else:
            self.atom += c

    def parse(self):
        while not self.io.eof():
            c = self.io.getchar()
            self.state(c)

        if len(self.stack) == 1:
            return self.stack[0]
        else:
            raise Exception("error: list not closed")


# EOF #
