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

import argparse


class FlexlayArgumentManager:
    """Put simply, a wrapper for argparse

    Needed because some arguments may be reserved by FlexAPI
    """
    def __init__(self, *parser_args):
        """Create a new manager

        Parameters:
            *parser_args - all passed into new ArgumentParser instance.
        """
        self.argument_parser = argparse.ArgumentParser(*parser_args)

    def add_argument(self, *args):
        pass

    def parse_arguments(self):
        pass
