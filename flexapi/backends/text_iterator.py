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

class TextIterator:
    """A simple class for iterating over text

    This class is useful when creating a backend if you wish to iterate over
    text and find regular expressions.

    Members:
    The TextIterator has 5 key members:
    text - The text being iterated over
    char - The current character which the iterator is at
    index - The current index of the iterator.
            TextIterator.text[TextIterator.index] = TextIterator.char
    accepted - The most recently accepted string.
    done - Whether the iterator has finished.
    """
    def __init__(self, text):
        """Create a new TextIterator instance"""
        self.text = text
        self.char = ""
        self.index = 0
        self.accepted = ""
        self.done = False
        self.set_index(0)

    def set_index(self, index):
        """Set the current index in the text.

        Use this, to ensure that the iterator updates properly.
        Entering a value below 0 will raise an IndexError.
        Entering a value above len(text) will raise an IndexError.
        Entering the value len(text) will stop all iteration and set done to
        True
        """
        if not self.done:
            if len(self.text) > index and index >= 0:
                self.index = index
                self.char = self.text[index]
            elif index == len(self.text):
                self.done = True
                self.char = ""
                self.index = -1
            else:
                raise IndexError("TextIterator reached index outside of text"\
                                 " passed to it")

    def step(self):
        """Step to the next char in the text

        Stepping too far will raise an IndexError.
        """
        self.set_index(self.index + 1)

    def steps(self, number):
        """Step a number of times

        Negative numbers will step backwards.
        Stepping too far will raise an IndexError.
        """
        self.set_index(self.index + number)

    def step_back(self):
        """Equivalent to steps(-1)"""
        self.set_index(self.index - 1)

    def ignore_regex(self, regex):
        """Jump the index over and beyond a match to the regex.

        regex must be compiled using re.compile(string_pattern)
        "accepted" will not be updated. Use accept_regex() to store skipped text
        """
        match = regex.match(self.text[self.index:])
        if match is not None:
            self.set_index(match.end() + self.index)

    def accept_string(self, string):
        """Jump the index over and beyond a string, if next in the text

        If a match is found, string will be stored in "accepted".
        """
        does_match = True
        check_index = self.index
        for char in string:
            try:
                if char == self.text[check_index]:
                    check_index += 1
                else:
                    does_match = False
                    break
            except IndexError:
                does_match = False
                break
        if does_match:
            self.accepted = string
            self.set_index(check_index)

    def accept_regex(self, regex):
        """Jump the index over and beyond a regex match, if next in the text

        If a match is found, string will be stored in "accepted".
        """
        match = regex.match(self.text[self.index:])
        if match is not None:
            self.accepted = self.text[match.start() + self.index:
                                      match.end() + self.index]
            self.set_index(match.end() + self.index)
    