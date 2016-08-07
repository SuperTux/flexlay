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


class FlexlayElement:
    """The base class of all elements

    An Element is a part of the Flexlay window, for example a dock widget, or a menubar.
    You can extend this class to create your own, or use one of those already created.
    """
    def __init__(self):
        """Element constructor"""
        self.editor = None

    def added_to(self, editor):
        """This method is executed when the element is added to its editor. And also this is a test

        Each element can only be added to a single editor (Why would you add the same one to two?!).
        Please override this, but call super().add(editor) at the top of the method
        Parameters:
            editor - The editor to which this element will be added. (FlexlayEditor)
        """
        assert self.editor is None, "Cannot add element which has already been added to an editor."
        self.editor = editor

    def removed(self):
        """This method is executed when the element is removed from its editor.

        Please override this, but call super().remove() at the top of the method
        """
        assert self.editor is not None, "Unable to remove element which is not currently part of an editor"
        self.editor = None
