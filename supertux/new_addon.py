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


import random

from PyQt5 import QtGui

from flexlay import Config
from flexlay.gui import PropertiesWidget, GenericWizard
from supertux.addon import Addon


class NewAddonWizard(GenericWizard):
    def __init__(self, parent):
        super().__init__(parent, "New Addon Wizard")
        # Modal means you cannot touch the parent until this
        # closes
        self.setModal(True)

        self.addon = Addon()
        self.addon.id = "unnamed-addon"
        self.addon.version = 1
        self.addon.type = "worldmap"
        self.addon.title = "New add-on"
        self.addon.author = Config.current.name
        self.addon.license = "GPL 2+ / CC-by-sa 3.0"
        # self.level.spawn = (5, 25)

        # Each function returns a QWizardPage, which is then added
        self.add_page("Create A New Add-on", self.create_intro_page())
        self.add_page("General Info", self.create_main_page())
        self.addPage(self.create_license_page())

        # When 'Finish' pressed run finish()
        self.finish_callback.connect(self.finish)
        # When 'Cancel' pressed run cancel()
        self.rejected.connect(self.cancel)

    def finish(self, *pages):
        """Executed when "Finish" button clicked"""
        main_page_data = pages[1]
        self.addon = Addon()
        self.addon.title = main_page_data[0]
        self.addon.author = main_page_data[1]
        self.addon.version = main_page_data[2]
        self.addon.id = main_page_data[3]
        self.addon.type = main_page_data[4]

        # self.level.current_sector.music = self.level.music
        # self.level.current_sector.background = self.level.image
        # self.level.name = main_page_data[0]
        # self.level.author = main_page_data[1]
        Config.current.name = self.addon.author
        # self.level.current_sector.music = main_page_data[4]
        # self.level.spawn = ceil(main_page_data[2] / 10), int(main_page_data[3] / 2)
        # self.level.tileset_path = os.path.join("images", "tiles.strf")

    # Not Implemented
    def cancel(self):
        self.addon = None

    def create_intro_page(self):
        """Creates the intro page containing a bit of text

        :return: PropertiesWidget Introduction Page
        """
        page_widget = PropertiesWidget(self)

        page_widget.add_label("Click 'Next' to continue.\n" +
                              "In the future, this page will show some " +
                              "checkboxes to allow experienced users " +
                              "to view extra pages in this wizard.")

        return page_widget

    def create_main_page(self):
        """Creates the main wizard page

        :return: PropertiesWidget to add to GenericWidget
        """
        page_widget = PropertiesWidget(self)

        # A list of names randomly selected as the 'Example
        # Level name.
        # Feel free to add more
        fun_names = ("Fire Meets Ice",
                     "Flipper Hell",
                     "Level 4",
                     "Into the Deep",
                     "Unknown Flying Penguin")

        # choice is random.choice
        self.addon.title = "Example: " + random.choice(fun_names)

        page_widget.add_string("Title:", self.addon.title, None)
        page_widget.add_string("Author:", self.addon.author, None)
        page_widget.add_int("Version:", self.addon.version, None)
        page_widget.add_string("ID:", self.addon.id, None)
        page_widget.add_string("Type:", self.addon.type, None)

        return page_widget

    def create_license_page(self):
        """Creates the license page

        @return QWizardPage The License Page of the wizard
        """
        page = QtGui.QWizardPage()

        page.setTitle("Addon License")
        page.setSubTitle("You must set a license for your level, which " +
                         "defines how others may use and share your level. " +
                         "In the spirit of 'free and open source' " +
                         "we ask that you make your level free " +
                         "(as in 'free speech' not 'free WiFi')")

        self.license_input = QtGui.QTextEdit("GPL 2+ / CC-by-sa 3.0")
        self.set_license()
        self.license_input.textChanged.connect(self.set_license)

        vbox = QtGui.QVBoxLayout()
        vbox.addWidget(self.license_input)

        page.setLayout(vbox)
        return page

    def set_license(self):  # Connected to signal
        self.addon.license = self.license_input.toPlainText()
