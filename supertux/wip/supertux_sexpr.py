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

from flexapi.resources.translatable_text_resource import TranslatableTextResource


class SuperTuxSexpr:
    """Currently read only"""
    def __init__(self, tree):
        self.tree = tree
        self.name = tree[0]
        self.items = tree[1:] if len(tree) >= 2 else []
        
    def __getitem__(self, key):
        if type(key) is not str:
            raise TypeError("key must be string")
        
        found_items = []
        for item in self.items:
            if len(item) > 0 and item[0] == key:
                if len(item) == 1:
                    found_items.append(None)
                    continue
                if type(item[1]) is list:
                    if len(item[1]) == 2 and item[1][0] == "_":
                        # For now, ignore the fact that it's translatable
                        found_items.append(item[1][1])
                        continue
                    else:
                        found_items.append(SuperTuxSexpr(item))
                        continue
                if len(item) == 2:
                    found_items.append(item[1])
                    continue
                found_items.append(item[1:])
        return found_items
    
    def __str__(self):
        return str(self.tree)
    

