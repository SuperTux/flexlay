# Flexlay - A Generic 2D Game Editor
#
# ISC License
# Copyright (C) 2016 Karkus476 <karkus476@yahoo.com>
#
# Permission to use, copy, modify, and/or distribute this software for
# any purpose with or without fee is hereby granted, provided that the
# above copyright notice and this permission notice appear in all
# copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
# WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
# AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR ON SEQUENTIAL
# DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
# PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
# TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
# PERFORMANCE OF THIS SOFTWARE.


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
