import os
import sys
import code
from flexlay import *
from sexpr   import *

class Level:
    version = 2
    filename = None
    
    name   = "no name"
    author = "no author"
    width  = 200
    height = 100
    gravity = 10
    theme = "antarctica"
    time = 999
    music = "Mortimers_chipdisko.mod"

    foreground  = None
    interactive = None
    background  = None
    
    objects = None
    camera  = None

    sectiors = None

    editormap = None

    def __init__(self, *params):
        if len(params) == 2:
            # New Level
            (width, height) = params
            
            self.name   = "No Name"
            self.author = "No Author"

            self.width  = width
            self.height = height

            self.foreground  = TilemapLayer(tileset, self.width, self.height)
            self.interactive = TilemapLayer(tileset, self.width, self.height)
            self.background  = TilemapLayer(tileset, self.width, self.height)
            self.objects = ObjectLayer()

            self.editormap = EditorMap()
            self.editormap.add_layer(self.background.to_layer())
            self.editormap.add_layer(self.interactive.to_layer())
            self.editormap.add_layer(self.objects.to_layer())
            self.editormap.add_layer(self.foreground.to_layer())
            
            # FIXME: Data might not get freed since its 'recursively' refcounted
            self.editormap.set_metadata(make_metadata(self))
            
        elif len(params) == 1:
            # Load Level from file
            (self.filename,) = params
            
            tree = sexpr_read_from_file(self.filename)
            if tree == None:
                raise ("Couldn't load level: ", filename)
            
            data = tree[1:]

            self.version = get_value_from_tree(["version", "_"], data, 1)

            if (self.version == 1):
                self.parse_v1(data)
            else:
                self.parse_v2(data)
        else:
            raise "Wrong arguments for SuperTux::___init__"

    def parse_v2(self, data):
        self.name    = get_value_from_tree(["name", "_"], data, "no name")
        self.author  = get_value_from_tree(["author", "_"], data, "no author")
        self.time    = int(get_value_from_tree(["time", "_"], data, "999"))
        
        self.sectors = []
        for sec in sexpr_filter("sector", data):
            sector = Sector(self, sec)
            self.sectors.append(sector)
            self.interactive = sector.tilemap
            self.background  = sector.tilemap
            self.foreground  = sector.tilemap
            self.objects     = sector.objects

        self.editormap = EditorMap()
        self.editormap.add_layer(self.interactive.to_layer())
        self.editormap.add_layer(self.objects.to_layer())
        # FIXME: Data might not get freed since its 'recursively' refcounted
        self.editormap.set_metadata(make_metadata(self))

    def parse_v1(self, data):
        self.name    = get_value_from_tree(["name", "_"], data, "no name")
        self.author  = get_value_from_tree(["author", "_"], data, "no author")
        self.time    = int(get_value_from_tree(["time", "_"], data, "999"))
        
        self.width  = get_value_from_tree(["width", "_"], data, 20)
        self.height = get_value_from_tree(["height""_"], data, 15)
        
        self.foreground  = TilemapLayer(tileset, self.width, self.height)
        self.foreground.set_data(get_value_from_tree(["foreground-tm"], data, []))
        
        self.interactive = TilemapLayer(tileset, self.width, self.height)
        self.interactive.set_data(get_value_from_tree(["interactive-tm"], data, []))
        
        self.background  = TilemapLayer(tileset, self.width, self.height)
        self.background.set_data(get_value_from_tree(["background-tm"], data, []))

        def find(lst, obj):
            for i in lst:
                if i[0] == obj:
                    return i
            return None
            
        self.objects = ObjectLayer()
        for i in get_value_from_tree(["objects"], data, []):
            type = i[0]
            x = get_value_from_tree(["x", "_"], i[1:], [])
            y = get_value_from_tree(["y", "_"], i[1:], [])
            object = find(game_objects, type)
            self.objects.add_object(ObjMapSpriteObject(make_sprite(config.datadir + object[1]),
                                                       CL_Point(x, y),
                                                       make_metadata(BadGuy(object[0]))).to_object())
                
        for i in get_value_from_tree(["reset-points"], data, []):
            type = i[0]
            x = get_value_from_tree(["x", "_"], i[1:], [])
            y = get_value_from_tree(["y", "_"], i[1:], [])
            object = find(game_objects, "resetpoint")
            self.objects.add_object(ObjMapSpriteObject(make_sprite(config.datadir + object[1]),
                                                       CL_Point(x, y),
                                                       make_metadata(BadGuy(object[0]))).to_object())
        self.editormap = EditorMap()
        self.editormap.add_layer(self.background.to_layer())
        self.editormap.add_layer(self.interactive.to_layer())
        self.editormap.add_layer(self.objects.to_layer())
        self.editormap.add_layer(self.foreground.to_layer())
        # FIXME: Data might not get freed since its 'recursively' refcounted
        self.editormap.set_metadata(make_metadata(self))
            
          
    def resize(self, size, pos):
        self.width  = size.width
        self.height = size.height
        self.background.resize(size, pos)
        self.interactive.resize(size, pos)
        self.foreground.resize(size, pos)
        
    def save(self, filename):
        self.save_v2(filename)

    def save_v2(self, filename):
        f = file(filename, 'w')
        f.write(";; Generated by Flexlay Editor\n"
                "(supertux-level\n")
        f.write("  (version 2)\n")
        f.write("  (name   \"%s\")\n" % self.name)
        f.write("  (author \"%s\")\n" % self.author)
        f.write("  (width  %s)\n"  % self.width)
        f.write("  (height  %s)\n" % self.height)

        f.write("  (music  \"%s\")\n" % self.music)
        f.write("  (time   \"%s\")\n" % self.time)

        f.write("  (gravity %d)\n" % self.gravity)

        f.write("  (theme \"%s\")\n" % self.theme)

        f.write("  (interactive-tm\n")
        for i in self.interactive.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")

        f.write("  (background-tm\n")
        for i in self.background.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")

        f.write("  (foreground-tm\n")
        for i in self.foreground.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")

        f.write("  (camera\n")
        f.write("    (mode \"autoscroll\")\n")
        f.write("    (path\n")
        for obj in self.objects.get_objects():
            pathnode = get_python_object(obj.get_metadata())
            if (pathnode.__class__ == PathNode):
                f.write("     (point (x %d) (y %d) (speed 1))\n" % (obj.get_pos().x, obj.get_pos().y))
        f.write("  ))\n\n")

        f.write("  (objects\n")
        for obj in self.objects.get_objects():
            badguy = get_python_object(obj.get_metadata())
            if (badguy.__class__ == BadGuy):
                pos    = obj.get_pos()
                if (badguy.type != "resetpoint"):
                    f.write("     (%s (x %d) (y %d))\n" % (badguy.type, int(pos.x), int(pos.y)))
        f.write("  )\n\n")

        f.write("  (reset-points\n")
        for obj in self.objects.get_objects():
            badguy = get_python_object(obj.get_metadata())
            if (badguy.__class__ == BadGuy):
                pos    = obj.get_pos()
                if (badguy.type == "resetpoint"):
                    f.write("     (point (x %d) (y %d))\n" % (int(pos.x), int(pos.y)))
        f.write("  )\n\n")
        
        f.write(" )\n\n;; EOF ;;\n")

    def save_v1(self, filename):
        f = file(filename, 'w')
        f.write(";; Generated by Flexlay Editor\n"
                "(supertux-level\n")
        f.write("  (version 1)\n")
        f.write("  (name   \"%s\")\n" % self.name)
        f.write("  (author \"%s\")\n" % self.author)
        f.write("  (width  %s)\n"  % self.width)
        f.write("  (height  %s)\n" % self.height)

        f.write("  (music  \"%s\")\n" % self.music)
        f.write("  (time   \"%s\")\n" % self.time)

        f.write("  (gravity %d)\n" % self.gravity)

        f.write("  (theme \"%s\")\n" % self.theme)

        f.write("  (interactive-tm\n")
        for i in self.interactive.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")

        f.write("  (background-tm\n")
        for i in self.background.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")

        f.write("  (foreground-tm\n")
        for i in self.foreground.get_data():
            f.write("%d " % i)
        f.write("  )\n\n")

        f.write("  (camera\n")
        f.write("    (mode \"autoscroll\")\n")
        f.write("    (path\n")
        for obj in self.objects.get_objects():
            pathnode = get_python_object(obj.get_metadata())
            if (pathnode.__class__ == PathNode):
                f.write("     (point (x %d) (y %d) (speed 1))\n" % (obj.get_pos().x, obj.get_pos().y))
        f.write("  ))\n\n")

        f.write("  (objects\n")
        for obj in self.objects.get_objects():
            badguy = get_python_object(obj.get_metadata())
            if (badguy.__class__ == BadGuy):
                pos    = obj.get_pos()
                if (badguy.type != "resetpoint"):
                    f.write("     (%s (x %d) (y %d))\n" % (badguy.type, int(pos.x), int(pos.y)))
        f.write("  )\n\n")

        f.write("  (reset-points\n")
        for obj in self.objects.get_objects():
            badguy = get_python_object(obj.get_metadata())
            if (badguy.__class__ == BadGuy):
                pos    = obj.get_pos()
                if (badguy.type == "resetpoint"):
                    f.write("     (point (x %d) (y %d))\n" % (int(pos.x), int(pos.y)))
        f.write("  )\n\n")
        
        f.write(" )\n\n;; EOF ;;\n")

    def activate_sector(self, sector, workspace):
        for sec in self.sectors:
            if sec.name == sector:
                workspace.set_map(sec.editormap)
                TilemapLayer.set_current(sec.tilemap)
                ObjectLayer.set_current(sec.objects)
                break

    def activate(self, workspace):
        workspace.set_map(self.editormap)
        TilemapLayer.set_current(self.interactive)
        ObjectLayer.set_current(self.objects)
        #(tilemap-paint-tool-set-tilemap (supertux:interactive-tm stlv))
        #(editor-tilemap-set-current     (supertux:interactive-tm stlv))
        #(editor-objectmap-set-current   (supertux:objmap stlv))
        #(set! *tilemap* (supertux:interactive-tm stlv))
        #(set! *objmap* (supertux:objmap stlv))
        #(tileset-set-current *level-tileset*)
        #(tile-selector-set-tileset *tileselector* *level-tileset*))

Level.BACKGROUND  = 0
Level.INTERACTIVE = 1
Level.FOREGROUND  = 2

# EOF #
