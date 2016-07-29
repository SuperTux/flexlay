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


class Signal:
    """A useful class for creating callbacks and handling events

    To use, create an instance, connect some subscribers (function callbacks to be run when the signal is triggered)
    using Signal.connect and then use as a callback or execute as a callable object
    """
    def __init__(self, *subscribers):
        """Create a new Signal

        Any number of parameters, each being a subscriber. Repeated entries will be cleaned up.
        """
        self.subscribers = []
        if len(subscribers) != 0:
            self.connect(*subscribers, ignore_repeats=False)

    def connect(self, *callbacks, ignore_repeats=True):
        """Add any number of callbacks to be triggered by the event

        Parameters:
            ignore_repeats - If False, any entries already connected to the Signal will be ignored.
        """
        for callback in callbacks:
            if not ignore_repeats and callback in self.subscribers:
                return
            self.subscribers.append(callback)

    def disconnect(self, callback):
        """Remove a callback from the signal"""
        self.subscribers.remove(callback)

    def clear(self):
        """Wipe all callbacks connected."""
        self.subscribers = []

    def __call__(self, *args):
        for sub in self.subscribers:
            sub(*args)

# EOF #
