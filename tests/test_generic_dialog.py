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


import sys
import unittest

from PyQt5.QtWidgets import QApplication

from flexlay.gui.generic_dialog import GenericDialog


class FlexlayGenericDialogTestCase(unittest.TestCase):
    """A set of test cases for GenericDialog

    In all cases, press okay to have the values entered printed
    Please note some of these features are still WIP.
    """

    def simple_test(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        GenericDialog("Generic Dialog Test")
        # test_app.exec()

    def test_label(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        test_dialog = GenericDialog("Generic Dialog Label Test")
        test_dialog.add_label("This is a test label")
        # test_app.exec()

    def test_string(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        test_dialog = GenericDialog("Generic Dialog String Test")
        test_dialog.add_string("String Label", "Initial Value")
        test_dialog.add_callback(print)
        # test_app.exec()

    def test_int(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        test_dialog = GenericDialog("Generic Dialog Test")
        test_dialog.add_int("Integer Label", 0)
        test_dialog.add_callback(print)
        # test_app.exec()

    def test_float(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        test_dialog = GenericDialog("Generic Dialog Float Test")
        test_dialog.add_float("Float Label", 1.0)
        test_dialog.add_float("Limit Test",
                              9.9999999999999999)
        test_dialog.add_callback(print)
        # test_app.exec()

    def test_enum(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        test_dialog = GenericDialog("Generic Dialog Enum Test")
        test_dialog.add_enum("Enum Label", ("Value 1", "Value 2", "Value 3"), 0)
        test_dialog.add_callback(print)
        # test_app.exec()

    def test_bool(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        test_dialog = GenericDialog("Generic Dialog Boolean Test")
        test_dialog.add_bool("Bool Label", True)
        test_dialog.add_callback(print)
        # test_app.exec()

    def test_file(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        test_dialog = GenericDialog("Generic Dialog File Path Test")
        test_dialog.add_file("File Path", "")
        test_dialog.add_callback(print)
        # test_app.exec()

    def test_radio(self):
        test_app = QApplication(sys.argv)  # noqa: F841
        test_dialog = GenericDialog("Generic Dialog Radio Buttons Test")
        test_dialog.add_radio("Radio Label", ("Value 1", "Value 2", "Value 3"), 0)
        test_dialog.add_callback(print)
        # test_app.exec()

# EOF #
