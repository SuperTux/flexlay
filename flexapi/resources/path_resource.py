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