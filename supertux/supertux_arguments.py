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

from flexlay.math import Point
from flexlay.util import Config


class SuperTuxArguments:
    """A container for the various command line arguments"""
    def __init__(self):
        self.run_level = None
        # Not the same as a spawn point
        # Currently does nothing.
        self.spawn_at = None
        self.record_demo_file = None
        self.play_demo_file = None

    def get_popen_arg(self):
        """Returns a list to be passed straight into subprocess.Popen

        :return: A list to be passed as an argument for Popen
        """
        run_level = [self.run_level]
        play_demo = ["--play-demo", self.play_demo_file] if self.record_demo_file is not None else []
        record_demo = ["--record-demo", self.record_demo_file] if self.record_demo_file is not None else []
        # Not implemented in SuperTux!
        # spawn_at = ["--spawn-pos",
        #             str(self.spawn_at.x), ",",
        #             str(self.spawn_at.y)] \
        #     if self.spawn_at is not None \
        #     else []
        return [Config.current.binary] + run_level + play_demo + record_demo #+ spawn_at
