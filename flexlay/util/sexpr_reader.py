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


def assoc_ref(lst, key):
    if lst == []:
        return None
    elif lst[0][0] == key:
        return lst[0][1:]
    else:
        return assoc_ref(lst[1:], key)


def get_value_from_tree(spec, tree, default):
    if spec == []:
        return tree
    elif spec == ['_']:
        # is it a translatable string?
        if isinstance(tree[0], list) and tree[0][0] == "_":
            return tree[0][1]
        else:
            return tree[0]
    elif tree == []:
        return default
    else:
        el = assoc_ref(tree, spec[0])
        if el is not None:
            return get_value_from_tree(spec[1:], el, default)
        else:
            return default


class SExprReader:

    def __init__(self):
      pass


# EOF #
