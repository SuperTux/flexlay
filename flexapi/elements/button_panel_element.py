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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.#

from PyQt4.QtGui import QToolBar
from PyQt4.QtCore import Qt

from flexlay.gui import ButtonPanel

from flexapi.util import Signal
from .flexlay_element import FlexlayElement


class ButtonPanelElement(FlexlayElement):
    """This FlexlayElement can be added to give the user an array of buttons to press

    To use, create an instance, add it to the editor, and then create new Button instances.
    Change the Button.icon, Button.signal (callback) and Button.hover to change the icon displayed, function called
    when the button is pressed and the text which appears when the user hovers the mouse over the button. To do this
    you can either create a Button instance or an instance of the other *Button classes provided in this module.
    """
    _buttons = {}

    button_panel = None

    def added_to(self, editor):
        toolbar = QToolBar()
        toolbar.setObjectName("button_panel")
        editor.window.addToolBar(Qt.TopToolBarArea, toolbar)
        self.button_panel = ButtonPanel(toolbar)

    def add_button(self, key, button):
        """Add a Button object to this button panel

        Please note that once a button is added it cannot be removed, only activated or deactivated.
        Parameters:
            key - Name by which button can be retrieved from panel. Use button_panel_element[key] to retrieve in future
            button - The Button object which will be added (Button)
        """
        assert isinstance(button, Button), "Cannot add button if not instance of [subclass of] Button"
        assert key not in self._buttons, "Key already used by another button, try another."
        assert self.button_panel is not None, "Cannot add button unless ButtonPanelElement has been added to an editor."
        self._buttons[key] = button
        self.button_panel.add_icon(button.icon, button.signal, button.hover, button.shortcut)

    def __getitem__(self, item):
        if type(item) != str:
            raise TypeError("Key must be string")
        if item not in self._buttons:
            raise IndexError("No such button with key '" + item + "'")
        return self._buttons[item]

    def __setitem__(self, key, value):
        if type(key) != str:
            raise TypeError("Key must be string")
        if key not in self._buttons:
            self.add_button(key, value)
        else:
            self._buttons[key] = value

    """
    def __delitem__(self, key):
        if type(key) != str:
            raise TypeError("Index must be string")
        if key not in self._buttons:
            raise IndexError("No such button with key '" + key + "'")
        self.remove_button(key)
    """


class Button:
    """Base class for all buttons added to the button panel element.

    Use one of the many already created, or subclass this.
    """

    def __init__(self, *subscribers):
        """Creates a new Button object

        Parameters:
            signal - A Signal object which will be called when the button is pressed
            subscribers - Any subscribers to be added to a new Signal object or the one provided as the first argument
        """
        self.icon = None
        self.signal = Signal()
        self.hover = None
        self.shortcut = None
        # The importance of this is that Qt adds arguments we can ignore. This connects indirectly through a lambda
        #  statement which will cause the arguments to be discarded
        self.signal.connect(*[lambda *args: sub() for sub in subscribers])


class SaveButton(Button):
    """Icon and hover text only"""
    def __init__(self, *subscribers):
        super().__init__(*subscribers)
        self.icon = "data/images/icons24/stock_save.png"
        self.hover = "Save"
        # shortcut = "Ctrl+S"


class OpenButton(Button):
    """Icon and hover text only"""

    def __init__(self, *subscribers):
        super().__init__(*subscribers)
        self.icon = "data/images/icons24/stock_open.png"
        self.hover = "Open"
        # shortcut = "Ctrl+0"


class NewButton(Button):
    """Icon and hover text only"""

    def __init__(self, *subscribers):
        super().__init__(*subscribers)
        self.icon = "data/images/icons24/stock_new.png"
        hover = "New"
        # shortcut = "Ctrl+N"


class SaveAsButton(Button):
    """Icon and hover text only"""
    pass


class UndoButton(Button):
    """Icon and hover text only"""
    pass


class RedoButton(Button):
    """Icon and hover text only"""
    pass


class CopyButton(Button):
    """Icon and hover text only"""
    pass


class CutButton(Button):
    """Icon and hover text only"""
    pass


class PasteButton(Button):
    """Icon and hover text only"""
    pass


class RunButton(Button):
    """Icon and hover text only"""
    pass
