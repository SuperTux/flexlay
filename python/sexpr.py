##  $Id$
## 
##  Flexlay - A Generic 2D Game Editor
##  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
##
##  This program is free software; you can redistribute it and/or
##  modify it under the terms of the GNU General Public License
##  as published by the Free Software Foundation; either version 2
##  of the License, or (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

def assoc_ref(lst, str):
    if lst == []:
        return False
    elif lst[0][0] == str:
        return lst[0][1:]
    else:
        return assoc_ref(lst[1:], str)

def sexpr_filter(name, tree):
    ret = []
    for i in tree:
        if i[0] == name:
            ret.append(i[1:])
    return ret

def get_value_from_tree(spec, tree, default):
    if spec == []:
        return tree
    elif spec == ['_']:
        return tree[0]
    elif tree == []:
        return default
    else:
        el = assoc_ref(tree, spec[0])
        if el:
            return get_value_from_tree(spec[1:], el, default)
        else:
            return default

# EOF #
