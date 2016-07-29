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

from ..resources.file_resource import FileResource

class EditorTab:
    """Subclass this to create your own tab types."""
    def __init__(self, item):
        """Opens a new tab with item as the value."""
        self.item = item

    @classmethod
    def can_edit(Tab, item):
        """Performs an ability check on item, which may any resource or possibly other type. (e.g. string)

        Should return value >0 if this tab can (is able to) provide editing functionality for this value/object.
        This function should return an integer indicating how effective this class is at editing these items.
        Not sure? Return 5 if can edit, and 0 if cannot.
        Quick Outline:
        0: Unable to edit, or subclasses should not be allowed to edit.
        1: Return by default if this is an abstract class, and checks succeed
        2 - 4: Return if not abstract class, and can edit
        5 - 7: Provides some specific features, subclasses may be made.
        8 - 10: Very specific to this resource, file type or data type
        Abstract base classes should return 1 by default and only return 0 if subclasses should not be allowed to
        edit.
        A super call can then be used to easily perform some checks.
        """
        return 1

    def get_widget(self):
        return None
