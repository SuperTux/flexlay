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

from ..flexlay_error import FlexlayError


class BackendError(FlexlayError):
    """For all errors backends might raise"""
    def __init__(self, message, filename=None, line_no=None, char_no=None, char=None):
        super().__init__(self._format_error(message, filename, line_no, char_no, char))
    
    def _format_error(self, message, filename, line_no, char_no, char):
        formatted = "\nError while reading "
        if line_no is not None:
            formatted += "line " + str(line_no)
            if char_no is not None:
                formatted += ", character no. " + str(char_no)
                if char is not None:
                    formatted += ": '" + char + "'"
            formatted += " of "
        formatted += "file"
        if filename is not None:
            formatted += " '" + str(filename) + "'"
        formatted += ".\n" + message
        return formatted
