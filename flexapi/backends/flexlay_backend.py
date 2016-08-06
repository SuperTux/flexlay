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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import flexlay.util as util


class FlexlayBackend:
    """The base class for all backends

    A backend is what Flexlay uses to save the levels your users will create.
    There are many different formats which
    have already been created, or you can make your own by using this base class.
    """
    def __init__(self, backend_name, backend_version=None):
        """FlexlayBackend constructor

        Parameters:
            backend_name - The name of this backend. For example "supertux-sexpr"
            backend_version - An integer or a flexlay.util.Version
        """
        self.name = backend_name
        self.version = backend_version
