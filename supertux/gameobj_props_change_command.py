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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See this
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


from flexlay.commands import Command


class GameObjPropsChangeCommand(Command):
    """
    This Command is run when a set of properties of an object are
    changed.
    """

    def __init__(self, gameobj, prop_diff):
        """It's probably not a good idea to use this for non-directly
        editable properties.
        Warning: This will accept any value

        :param gameobj: GameObj this affects
        :param prop_diff: list of tuples of the form:
                          (prop_index, execute_value, undo_value)
        """
        super().__init__()

        self.gameobj = gameobj
        self.prop_diff = prop_diff

    def execute(self):
        for diff in self.prop_diff:
            print(self.gameobj.properties[diff[0]].value, "=", diff[1])
            self.gameobj.properties[diff[0]].value = diff[1]

    def redo(self):
        for diff in self.prop_diff:
            self.gameobj.properties[diff[0]].value = diff[1]

    def undo(self):
        for diff in self.prop_diff:
            self.gameobj.properties[diff[0]].value = diff[2]


# EOF #
