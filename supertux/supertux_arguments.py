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


from typing import Optional

from flexlay.util.config import Config


class SuperTuxArguments:
    """A container for the various command line arguments of the SuperTux binary (not supertux-editor!)"""

    def __init__(self) -> None:
        self.run_level: Optional[str] = None
        # Not the same as a spawn point
        # Currently does nothing.
        self.spawn_at: Optional[str] = None
        self.record_demo_file: Optional[str] = None
        self.play_demo_file: Optional[str] = None

    def get_popen_arg(self) -> list[str]:
        """Returns a list to be passed straight into subprocess.Popen

        :return: A list to be passed as an argument for Popen
        """
        run_level = [self.run_level]
        play_demo = ["--play-demo", self.play_demo_file] if self.record_demo_file is not None else []
        record_demo = ["--record-demo", self.record_demo_file] if self.record_demo_file is not None else []
        developer_mode = ["--developer"]
        # Not implemented in SuperTux!
        # spawn_at = ["--spawn-pos",
        #             str(self.spawn_at.x), ",",
        #             str(self.spawn_at.y)] \
        #     if self.spawn_at is not None \
        #     else []
        assert current.binary is not None
        return [Config.current.binary] + run_level + play_demo + record_demo + developer_mode  # + spawn_at


# EOF #
