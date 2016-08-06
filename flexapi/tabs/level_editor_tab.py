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

from .editor_tab import EditorTab
from ..resources import TextFileResource
from ..flexlay_error import FlexlayError

class LevelEditorTab(EditorTab):
    """The tab which contains the actual level being edited."""
    def __init__(self, item):
        super().__init__(item)
        if not isinstance(item, TextFileResource):
            raise FlexlayError("'item' to be edited in the LevelEditorTab must be TextFileResource")

    @classmethod
    def can_edit(Tab, item):
        if isinstance(item, TextFileResource):
            return 3
        else:
            return 0