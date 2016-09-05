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

import time, re
from .supertux_sexpr import SuperTuxSexpr
from flexapi.backends import TextIterator, BackendError

class SuperTuxParser:
    """Parse SuperTux Sexpr files into a list"""
    def __init__(self, text, filename=None):
        self.filename = filename
        self.it = TextIterator(text)
        self.line_no = 1
       
    @staticmethod
    def from_path(path):
        with open(str(path), "r") as f:
            text = f.read()
        return SuperTuxParser(text, filename=path)
    
    empty_pattern = "(\s+|;[^\n]*\n)+"
    symbol = re.compile(r"[a-zA-Z-_0-9]+")
    empty = re.compile(empty_pattern)
    integer = re.compile(r"-?[0-9]+")
    _float = re.compile(r"-?[0-9]+\.[0-9]+")
    boolean = re.compile(r"#[tf]")
    string = re.compile(r'"(\\.|[^"\\])*"')
    field = re.compile(r"([0-9]+\s+)*[0-9]+")
    
    whitespace = re.compile(r"\s+")
    
    def parse(self, optimise=None):
        """Parse the input text into a SuperTuxSexpr object
        
        Parameters:
            optimise - Either "level", "tileset" or None.
                       When parsing level files it will be much
                       more efficient. If the wrong optimisation is
                       selected, the parse will fail.
        """
        return SuperTuxSexpr(self._parse(optimise))
    
    def _parse(self, optimise=None):
        """Recursive, private part"""
        tree = []
        # Ignore some whitespace (or a comment)
        self.it.ignore_regex(SuperTuxParser.empty)
        # Ensure our sexpression starts with an opening bracket
        if not self.it.accept_string("("):
            self.raise_error("S-Expression must begin with an opening bracket.")
        
        # Ignore some whitespace (or a comment)
        self.it.ignore_regex(SuperTuxParser.empty)
        
        # Parse the symbol at the start of each sexpr
        if not self.it.accept_regex(SuperTuxParser.symbol):
            self.raise_error("First element in S-Expression must be a symbol.")
        tree.append(self.it.accepted)
        
        # Level files' field optimisation
        if (optimise == "level" and self.it.accepted == "tiles"):
            tiles = []
            while not self.it.accept_string(")"):
                if not self.it.accept_regex(SuperTuxParser.field):
                    if not self.it.ignore_regex(SuperTuxParser.empty):
                        self.raise_error("Unexpected character while parsing field.")
                    continue
                tiles += SuperTuxParser.whitespace.split(self.it.accepted)
            print(tiles)
            tree.append(tiles)
            return tree

        # Parse the rest of the sexpression as a list
        while not self.it.accept_string(")"): # Until we find the end quote...
            if self.it.ignore_regex(SuperTuxParser.empty):
                pass
            elif self.it.accept_regex(SuperTuxParser._float):
                tree.append(float(self.it.accepted))
            elif self.it.accept_regex(SuperTuxParser.integer):
                tree.append(int(self.it.accepted))
            elif self.it.accept_regex(SuperTuxParser.boolean):
                # If #t, add True, otherwise, assume #f and add False
                tree.append(self.it.accepted[1] == "t")
            elif self.it.accept_regex(SuperTuxParser.string):
                # Remove quote marks and convert escape chars to real characters
                string_accepted = self.it.accepted[1:-1]
                string_accepted = string_accepted.replace("\\n", "\n")
                string_accepted = string_accepted.replace("\\t", "\t")
                string_accepted = string_accepted.replace("\\\\", "\\")
                tree.append(string_accepted)
            elif self.it.accept_string("("):
                # Step back into position
                self.it.step_back()
                # Parse recursively
                tree.append(self._parse(optimise))
            else:
                self.raise_error("No valid list element found.")
        
        return tree
    
    def raise_error(self, message):
        raise BackendError(message, self.filename, self.it.line_no, self.it.char_no, self.it.char)

