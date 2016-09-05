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

from flexapi.util import SemVer


class FlexlayBackend:
    """The base class for all backends

    A backend is a helper class to allow you to get data from
    a file. A backend returns the data in the file, but it is
    up to the editor developer(s) to make use of it.
    There are many different formats which
    have already been created, or you can make your own by using this base class.
    """
    version = SemVer(0, 0, 0)

    def read(self, file_resource):
        """Read a file input as a file resource

        Return data contained within
        """
        pass

    def write(self, data, path_resource):
        """Write data back to a file.

        Parameters:
            data - The same format as parse returns
            path_resource - The filesystem location where the data should be written
        """
        pass
