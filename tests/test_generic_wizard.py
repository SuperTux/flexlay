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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import unittest, sys

from PyQt4.QtGui import QApplication

from flexlay.gui.generic_wizard import GenericWizard
from flexlay.gui.properties_widget import PropertiesWidget


class FlexlayGenericWizardTestCase(unittest.TestCase):
    def simple_test(self):
        test_app = QApplication(sys.argv)

        test_wizard = GenericWizard(None, "Generic Wizard Test")

        page1 = PropertiesWidget(test_wizard)
        page1.add_label("Hello World!")
        page1.add_string("Type:", "Initial Value", (lambda val: print(val)))
        test_wizard.add_page("Page 1", page1)

        page2 = PropertiesWidget(test_wizard)
        page2.add_label("Hello World Again!")
        test_wizard.add_page("Page 2", page2)

        test_wizard.add_callback(print)

        test_wizard.show()

        test_app.exec()
