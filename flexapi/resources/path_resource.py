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

from .flexlay_resource import FlexlayResource


class PathResource(FlexlayResource):
    """Any resource defined by a path, for example FileResource and DirectoryResource.

    No checks are made to ensure validity, so path may or may not exist.
    """
    def __init__(self, path=None):
        """Create a new PathResource, optionally from a path

        Parameters:
            path - A tuple defining the full path to the resource  E.g. ("/home", "user", "tests", "test")
        """
        if path is not None:
            self.set_path(path)
        else:
            self._path = None

    def set_path(self, path):
        """Set the path of this resource to a different value

        Parameters:
            path - A tuple defining the full path to the resource  E.g. ("/home", "user", "tests", "test")
        """
        if type(path) is not tuple:
            raise TypeError("PathResource does not accept path type: " + str(type(path)))
        self._path = path

    def get_path(self):
        """Returns the current path as a tuple. Use str(path_resource) to get this value as a string."""
        return self._path

    def __setattr__(self, key, value):
        if key == "path":
            self.set_path(value)
        else:
            super().__setattr__(key, value)

    def __getattr__(self, item):
        if item == "path":
            return self.get_path()
        else:
            return super().__getattribute__(item)

    def __str__(self):
        return os.path.join(*self._path)

    def __add__(self, other):
        if not isinstance(other, PathResource):
            super().__add__(other)
        else:
            self._path = tuple(list(self._path) + list(other._path))
