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

import os
import re

from flexapi import ResourceError
from .flexlay_resource import FlexlayResource


class PathResource(FlexlayResource):
    """Any resource defined by a path, for example FileResource and DirectoryResource.

    No checks are made to ensure validity, so path may or may not exist.
    """
    def __init__(self, path=None):
        """Create a new PathResource, optionally from a path

        Parameters:
            path - A tuple defining the full path to the resource  E.g. ("/home", "user", "tests", "test"),
                   or a string, which may include the unexpanded user character "~".
                   May also be another PathResource.
        """
        self._path = None
        if path is not None:
            if isinstance(path, PathResource):
                self.set_path(path.get_path())
            else:
                self.set_path(path)

    def set_path(self, path):
        """Set the path of this resource to a different value

        Parameters:
            path - A tuple defining the full path to the resource  E.g. ("/home", "user", "tests", "test")
        """
        if type(path) is list:
            self._path = path
        elif type(path) is str:
            self._path = PathResource.split(path)
        else:
            raise ResourceError("Invalid path type provided, must be string or list")

    def get_path(self):
        """Returns the current path as a tuple. Use str(path_resource) to get this value as a string."""
        return self._path

    def __str__(self):
        return os.path.join(*self._path)

    def __add__(self, other):
        if not isinstance(other, PathResource):
            raise TypeError("Cannot add this value to a path resource.")
        else:
            return PathResource(self._path + other._path)
            
    @staticmethod
    def split(string):
        """Convert a string path to a tuple path"""
        string = os.path.expanduser(string)
        # Remove slash at the end
        if string[-1] == "/" or string[-1] == "\\":
            string = string[:-1]
        preceding_slash = False
        if len(string) >= 1 and string[0] == "/":
            preceding_slash = True
            if len(string) > 1:
                string = string[1:]
            else:
                string = ""
        split_list = re.split("[\\/]", string)
        if preceding_slash:
            split_list[0] = "/" + split_list[0]
        return list(split_list)
