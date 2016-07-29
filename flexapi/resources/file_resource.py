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
            return self.path[-1]
        elif item == "extension":
            split = self.path[-1].split(".")
            if len(split) > 1:
                return split[-1]
            else:
                return ""
        else:
            return super().__getattr__(item)