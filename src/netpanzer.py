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

from flexlay import *
import netpanzertiles

tileset = Tileset(32)
netpanzertiles.load_netpanzer_tiles(tileset)

class Level:
    filename  = None
    data      = None
    editormap = None
    objects   = None

    def __init__(self, *params):
        if len(params) == 2:
            (width, height) = params
            self.data = NetPanzerFileStruct(tileset, width, height)

        elif len(params) == 1:
            (self.filename,) = params
            self.data = NetPanzerFileStruct(tileset, self.filename)

        self.objects = ObjectLayer()
        self.editormap = EditorMap()
        self.editormap.add_layer(self.data.get_tilemap().to_layer())
        self.editormap.add_layer(self.objects.to_layer())

        # FIXME: Data might not get freed since its 'recursively' refcounted
        self.editormap.set_metadata(make_metadata(self))

    def activate(self, workspace):
        workspace.set_map(self.editormap)
        TilemapLayer.set_current(self.data.get_tilemap())
        ObjectLayer.set_current(self.objects)

# EOF #
