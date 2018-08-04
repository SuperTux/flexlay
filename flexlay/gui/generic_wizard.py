# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2015 Karkus476 <karkus476@yahoo.com>
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
# along with this program.  If not, see <http://www.gnu.org/licenses/>


from PyQt5.QtWidgets import QWizardPage, QWizard, QVBoxLayout

from flexlay.util import Signal


class GenericWizard(QWizard):
    """A Wizard which can display properties easily

    You must use new_property_page to create pages which
    are made up of a PropertyWidget.

    Use add_callback to add a function which will be called
    when "Finish" is pressed. See that function for more details
    """
    def __init__(self, parent, title):
        super().__init__(parent)
        self.setWindowTitle(title)
        self.pages = []

        self.finish_callback = Signal()

        def on_finish():
            self.finish_callback(*self.get_values())
            for page in self.pages:
                page.call()
            self.hide()

        def on_cancel():
            self.hide()

        self.finished.connect(on_finish)
        self.rejected.connect(on_cancel)

    def add_page(self, title, widget):
        """Adds a page to this GenericWizard

        :param page: PropertyWidget to add
        """
        self.pages.append(widget)

        page = QWizardPage(self)
        page.setTitle(title)
        layout = QVBoxLayout()
        layout.addWidget(widget)
        page.setLayout(layout)

        self.addPage(page)

    def get_values(self):
        """ Returns a list of lists of all values put into this Wizard

        Best explained by example:
        my_wizard.get_values()[page_index][property_index]

        :return: A list of lists ^
        """
        result = []

        for page in self.pages:
            value = page.get_values()
            if value is not None:
                result.append(value)

        return result
