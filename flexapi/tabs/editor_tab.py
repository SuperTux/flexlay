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


class EditorTab:
    """Subclass this to create your own tab types."""
    def __init__(self, item):
        """Opens a new tab with item as the value."""
        self.item = item

    @classmethod
    def can_edit(Tab, item):
        """Performs an ability check on item, which may any resource or possibly other type. (e.g. string)

        Should return value >0 if this tab can (is able to) provide editing functionality for this value/object.
        This function should return an integer indicating how effective this class is at editing these items.
        Not sure? Return 5 if can edit, and 0 if cannot.
        Quick Outline:
        0: Unable to edit, or subclasses should not be allowed to edit.
        1: Return by default if this is an abstract class, and checks succeed
        2 - 4: Return if not abstract class, and can edit
        5 - 7: Provides some specific features, subclasses may be made.
        8 - 10: Very specific to this resource, file type or data type
        Abstract base classes should return 1 by default and only return 0 if subclasses should not be allowed to
        edit.
        A super call can then be used to easily perform some checks.
        """
        return 1

    def get_widget(self):
        return None
