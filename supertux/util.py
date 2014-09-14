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


from flexlay import Sprite
from flexlay.util import sexpr_read_from_file


def load_lisp(filename, root_symbol):
    """Convenience function that loads a lisp file from disk and checks for a
    root symbol"""

    tree = sexpr_read_from_file(filename)
    if tree is None:
        raise Exception("Error: Couldn't load '%s'" % filename)
    else:
        if tree[0] != root_symbol:
            raise Exception("Error: '#{filename}' is not a '%s' file" % root_symbol)
        else:
            return tree


def load_cl_sprite(filename):
    if filename[-4:] == ".png":
        sprite = Sprite.from_file(filename)
    elif filename[-7:] == ".sprite":
        supertux_sprite = Sprite(filename)
        sprite = supertux_sprite.get_cl_sprite()
    else:
        raise Exception("Unsupported sprite format '%s'" % filename)

    return sprite


# EOF #
