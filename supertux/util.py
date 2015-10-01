# Flexlay - A Generic 2D Game Editor
# Copyright (C) 2014 Ingo Ruhnke <grumbel@gmail.com>
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


from flexlay.util import sexpr_read_from_file


def load_lisp(filename, root_symbol):
    """Convenience function that loads a lisp file from disk and checks for a
    root symbol"""

    tree = sexpr_read_from_file(filename)[0]
    if tree is None:
        raise Exception("Error: Couldn't load '%s'" % filename)
    else:
        if tree[0] != root_symbol:
            raise Exception("Error: '%s' is not a '%s' file" % (filename, root_symbol))
        else:
            return tree


# EOF #
