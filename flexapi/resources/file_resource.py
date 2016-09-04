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

import os.path

from .path_resource import PathResource


class FileResource(PathResource):
    """A FileResource is a path to a file on the user's computer.

    The file must exist on the computer of the user, else the FileResource will not initialise.
    Members:
        filename - the last item in the path
        extension - the part after the "." in the file name. If no extension exists, then this value will be ""
    """
    def set_path(self, path):
        super().set_path(path)
        assert os.path.isfile(str(self)), "Failed to assign file resource to nonexistent file: " + str(self)

    def __getattr__(self, item):
        if item == "filename":
            return self._path[-1]
        elif item == "extension":
            split = self._path[-1].split(".")
            if len(split) > 1:
                return split[-1]
            else:
                return ""
        else:
            return super().__getattribute__(item)
