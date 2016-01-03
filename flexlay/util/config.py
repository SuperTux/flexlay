# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
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


import configparser
import os
import logging


class Config:
    current = None

    @staticmethod
    def create(projectname):
        path = os.path.expanduser("~/.flexlay/")
        if not os.path.isdir(path):
            os.mkdir(path)

        return Config(projectname, os.path.join(path, projectname + ".cfg"))

    def __init__(self, project_name, filename):
        Config.current = self

        # Equivalent to self.__dict__
        # We can't use self.attributes = {} as __setattr__ asks for self.attributes!
        self.__dict__["attributes"] = {}

        self.project_name = project_name
        self.filename = filename

        # Should be relevant to all editors.
        self.create_attribute("geometry", "")
        self.create_attribute("window_state", "")

        # Recent files is organised separately.
        self.recent_files = []

    def load(self, filename=None):
        if filename is None:
            filename = self.filename

        parser = configparser.ConfigParser()
        parser.read(filename)
        if self.project_name in parser:
            # Run through attributes, checking if they're in the parser's loaded variables.
            for key in self.attributes:
                if key in parser[self.project_name]:
                    self.attributes[key] = parser[self.project_name][key]

            # Load recent files, listed in order as recent_file# = filename
            self.recent_files = []
            for i in range(0, 10):
                if ('recent_files%d' % i) in parser[self.project_name]:
                    recent_file = parser[self.project_name]['recent_files%d' % i]
                    self.recent_files.append(recent_file)
        else:
            logging.info("No " + self.project_name + " section found in " + filename)

    def save(self, filename=None):
        if filename is None:
            filename = self.filename

        parser = configparser.ConfigParser()
        parser[self.project_name] = {}
        for key in self.attributes:
            parser[self.project_name][key] = self.attributes[key]

        for i, recent_file in enumerate(self.recent_files):
            parser[self.project_name]['recent_files%d' % i] = recent_file

        with open(filename, "w") as fout:
            parser.write(fout)

    def create_attribute(self, name, default_value):
        if name not in self.attributes and name not in self.__dict__:
            self.attributes[name] = default_value
        else:
            raise Exception("Cannot create attribute '" + name + "', it already exists!")

    def __setattr__(self, key, value):
        if key in self.attributes:
            self.attributes[key] = value
        else:
            super().__setattr__(key, value)

    def __getattr__(self, name):
        if name in self.__dict__:
            return self.__dict__[name]
        elif name in self.attributes:
            return self.attributes[name]
        else:
            raise AttributeError("Config instance has no such attribute '" + name + "'")

    def add_recent_file(self, filename):
        """Add/move filename to top of recent files list.

        Max in list is 10, so remove first filename
        if necessary
        :param filename: file to add to list
        """
        if filename in self.recent_files:
            # Remove from list, to be appended at the end
            self.recent_files.pop(self.recent_files.index(filename))
        else:
            # Ensure only 9 recent files
            while len(self.recent_files) >= 10:
                self.recent_files.pop(0)

        self.recent_files.append(filename)

# EOF #
