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

import sys; sys.path.append("..")
from supertux.wip.supertux_parser import SuperTuxParser



class SuperTuxTileset:
    def __init__(self, sexpr):
        # self.tilegroup_sexprs = sexpr["tilegroup"]
        self.tiles_sexprs = sexpr["tiles"]
        for tiles in self.tiles_sexprs:
            self.handle_tiles(tiles)
        # self.tile_sexprs = sexpr["tile"]
        
    def handle_tiles(self, tiles_sexpr):
        width = tiles_sexpr["width"][0]
        height = tiles_sexpr["height"][0]
        images = tiles_sexpr["images"][0]
        ids = tiles_sexpr["ids"][0]
        print(ids)
        
    @staticmethod
    def from_path(path):
        # Grumbel's time taken to parse tiles.strf:
        # 0.1804509162902832 seconds
        # Karkus' time taken to parse tiles.strf:
        # 2.752284526824951 seconds
        # Which is 15.25 times slower.
        parser = SuperTuxParser.from_path(path)
        sexpr = parser.parse()
        return SuperTuxTileset(sexpr)
    
if __name__ == "__main__":
    from flexapi.resources import FileResource, PathResource
    from supertux.supertux import editor_datadir
    path = editor_datadir + PathResource("supertux/tileset.strf")
    resource = FileResource(path)
    SuperTuxTileset.from_path(resource)
    
