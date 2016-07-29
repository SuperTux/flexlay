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


class FlexlayElement:
    """The base class of all elements

    An Element is a part of the Flexlay window, for example a dock widget, or a menubar.
    You can extend this class to create your own, or use one of those already created.
    """
    def __init__(self):
        """Element constructor"""
        self.editor = None

    def added_to(self, editor):
        """This method is executed when the element is added to its editor. And also this is a test

        Each element can only be added to a single editor (Why would you add the same one to two?!).
        Please override this, but call super().add(editor) at the top of the method
        Parameters:
            editor - The editor to which this element will be added. (FlexlayEditor)
        """
        assert self.editor is None, "Cannot add element which has already been added to an editor."
        self.editor = editor

    def removed(self):
        """This method is executed when the element is removed from its editor.

        Please override this, but call super().remove() at the top of the method
        """
        assert self.editor is not None, "Unable to remove element which is not currently part of an editor"
        self.editor = None
