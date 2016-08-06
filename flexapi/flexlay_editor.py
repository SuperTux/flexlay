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

from PyQt4.QtCore import QCoreApplication
from PyQt4.QtGui import QMainWindow, QApplication

import flexlay.util as util
from flexapi.elements.flexlay_element import FlexlayElement
from flexapi.flexlay_resource_manager import FlexlayResourceManager


class FlexlayMainWindow(QMainWindow):
    """The Qt instance of the editor window. Used by FlexlayEditor, no need to create your own"""
    close_signal = util.Signal()

    def closeEvent(self, event):
        """Qt callback"""
        self.close_signal()
        event.accept()


class FlexlayEditor():
    """The base class for all Flexlay editors

    Please extend this class in order to create your own editor.

    Quick Reminder: It's Python culture not to access instance variables or functions which start with an underscore.
    Accessing these may be required, but use is not documented and therefore discouraged. If a getter or setter is
    provided, of course you are free use them.

    Members:
        resource_manager - FlexlayResourceManager instance for this editor. Manages resources.
        window - Qt QMainWindow instance
        name - The name this wediotr will use for config files and projects. It's not recommended that this is changed
    """
    def __init__(self, editor_name, version):
        """Create a new editor

        Parameters:
            editor_name - The name of the editor, which will be used by the data directories and projects created.
                Important: Underscores or dashes, not spaces, lowercase.
                This will also become the initial value of the title bar, once formatted.
            version - Either an int or a flexlay.util.version.Version object
        """
        self.window = FlexlayMainWindow()
        self.name = editor_name
        # Simply converts "simple-editor" or "simple_editor" to "Simple Editor"
        self.set_title("".join([" " if  c == "-" or c == "_" else c for c in self.name]).title())
        self.version = version
        self.resource_manager = FlexlayResourceManager(self)
        self._elements = {}

    def add_element(self, key, element):
        """Add an element to the Editor

        Mostly to be done before starting the app.
        Creates a member of the instance for reference, by name given in 'key'
        Parameters:
            key - Name by which the element can later be accessed. This can be accessed by editor[key]. (str)
            element - The object to be added to the editor. Type must inherit FlexlayElement! (FlexlayElement)
        """
        assert isinstance(element, FlexlayElement), "Cannot add element if not instance of [subclass of] FlexlayElement"
        assert type(key) is str, "Element key must be string!"
        assert key not in self._elements, "Key already used by other element"
        self._elements[key] = element
        # Run init code...
        element.added_to(self)

    def remove_element(self, key):
        """Remove an element from the editor

        Parameters:
            key - The name by which the element is accessed.
        """
        assert key in self._elements, "Cannot remove element which is not in this editor!"
        self._elements[key].removed()
        del self._elements[key]

    def add_cardinal_elements(self):
        """Add the most basic of elements, which most editors will require

        This includes:
        ClassName (element_key): description
        """
        pass

    def set_title(self, title):
        """Change the editor window's title

        Parameters:
            title - The new window title
        """
        self.window.setWindowTitle(title)

    def run(self):
        """Make window visible to user, start the application."""
        self.window.show()
        QApplication.instance().exec_()

    def quit(self):
        """End the application, safely"""
        QCoreApplication.quit()

    def __getitem__(self, item):
        if type(item) != str:
            raise TypeError("Index must be string")
        return self._elements[item]

    def __setitem__(self, key, value):
        if type(key) != str:
            raise TypeError("Index must be string")
        if key not in self._elements:
            self.add_element(key, value)
        else:
            self._elements[key] = value

    def __call__(self):
        self.run()
