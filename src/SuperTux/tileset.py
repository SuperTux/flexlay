# converted to ruby
from flexlay import *
from sexpr   import *

def Tileset_load(self, filename):
    "Load game tiles from filename into tileset"
    tree = sexpr_read_from_file(filename)
    tree = tree[1:]
    for i in tree:
        if i[0] == "tile":
            data  = i[1:]
            id    = get_value_from_tree(['id', '_'], data, -1)
            image = get_value_from_tree(['editor-images', '_'], data, False)

            if not(image):
                image = get_value_from_tree(['images', '_'], data, "notile.png")

            if id != 0: # leave tile 0 transparent
                self.add_tile(id,
                              Tile(config.datadir + 'images/tilesets/' + image,
                                      CL_Color(255,   0,   0, 128)))
Tileset.load = Tileset_load
del Tileset_load

# EOF #
