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

from .flexlay_element import FlexlayElement


class MenubarElement(FlexlayElement):
    """Adds a menubar to the top of the editor"""
    menubar = None

    def added_to(self, editor):
        super().added_to(editor)
        # QMenuBar Instance
        self.menubar = editor.window.menuBar()

    def removed(self):
        super().removed()
        self.menubar = None

    def add_menu(self, label):
        """Adds a label to the menubar

        Returns a Menu object.
        See the Menu documentation for details
        """
        return Menu(self.menubar.addMenu(label))


class Menu:
    def __init__(self, menu):
        # QMenu Instance
        self.menu = menu

    def add_menu(self, label):
        """Create a new submenu, which pops out on hover.

        Parameters:
            label - The visible text on the menu.

        Returns a Menu object
        See the Menu documentation for more details
        """
        menu = self.menu.addMenu(label)
        return Menu(menu)

    def add_callback(self, label, callback):
        """Adds a clickable callback to the menu

        Parameters:
            label - The visible text in the menu
            callback - The method or callable object to be
                run when the label is clicked,
        """
        # QAction instance
        action = self.menu.addAction(label)
        if callback:
            # lambda stops arguments being passed to callback by Qt
            action.triggered.connect(lambda *args: callback())
