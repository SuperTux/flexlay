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


from typing import Any, Callable


SignalCallback = Callable[..., None]


class Signal:

    def __init__(self) -> None:
        self.subscribers: list[SignalCallback] = []

    def connect(self, callback: SignalCallback, ignore_repeats: bool = True) -> None:
        if not ignore_repeats and callback in self.subscribers:
            return
        self.subscribers.append(callback)

    def disconnect(self, callback: SignalCallback) -> None:
        self.subscribers.remove(callback)

    def clear(self) -> None:
        self.subscribers = []

    def __call__(self, *args: Any) -> None:
        for sub in self.subscribers:
            sub(*args)


# EOF #
