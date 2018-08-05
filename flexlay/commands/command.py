# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


class Command:
    """
    Commands allow undoable actions. This means,
    actions which can be undone by pressing the
    undo button. Create a new Command for each
    undoable action that may happen. They are
    then added to the undo stack. When undone,
    they are added to the redo stack
    """

    def __init__(self):
        """Here, the values involved in the
        Command should be initialised or
        passed as parameters"""
        pass

    def execute(self):
        """This method is run once - when the
        action happens."""
        pass

    def redo(self):
        """This method is run when this command
        is top of the undo stack and the undo
        button is clicked"""
        pass

    def undo(self):
        """This method is run when this command
        is top of the redo stack and the redo
        button is clicked"""
        pass

# EOF #
