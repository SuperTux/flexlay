class Sector:
    parent    = None
    name      = None
    song      = None
    gravity   = 10.0

    width  = None
    height = None

    background  = None
    interactive = None
    foreground  = None
        
    objects   = None
    editormap = None
    
    def __init__(self, parent):
        self.parent = parent

    def get_level(self):
        return self.parent

    def new(self, width, height):
        self.name = "<No Name>"
        self.song = "<No Song>"
        self.gravity = 10.0

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
        return self

    def load_v1(self, data):
        self.name = "<No Name>"
        self.song = "<No Song>"
        self.gravity = 10.0
        
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
            if object != None:
                self.objects.add_object(ObjMapSpriteObject(make_sprite(config.datadir + object[1]),
                                                           CL_Point(x, y),
                                                           make_metadata(BadGuy(object[0]))).to_object())
            else:
                print "Error: Couldn't resolve object type: ", type
                
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

    def load_v2(self, data):
        self.name = "<No Name>"
        self.song = "<No Song>"
        self.gravity = 10.0

        self.objects = ObjectLayer()
        for i in data:
            (name,data) = i[0], i[1:]
            if name == "name":
                self.name = data[0]
            elif name == "gravity":
                self.gravity = int(data[0])
            elif name == "playerspawn":
                print "playerspawn unhandled"
            elif name == "tilemap":
                width   = get_value_from_tree(["width", "_"], data, 20)
                height  = get_value_from_tree(["height", "_"], data, 15)
                solid   = get_value_from_tree(["solid", "_"], data, False)

                tilemap = TilemapLayer(tileset, width, height)
                tilemap.set_data(get_value_from_tree(["tiles"], data, []))

                print "Solid: ", solid
                if solid and self.interactive == None:
                    self.interactive = tilemap
                    self.width       = width
                    self.height      = height
                elif self.background == None:
                    self.background = tilemap
                elif self.foreground == None:
                    self.foreground = tilemap
                else:
                    print "Error: Duplicate tilemap in levelfile"
            elif name == "background":
                print "background unhandled"
            else:
                def find(lst, obj):
                    for i in lst:
                        if i[0] == obj:
                            return i
                    return None

                object = find(game_objects, name)
                if object != None:
                    (name, image) = object
                    x = get_value_from_tree(["x", "_"], data, [])
                    y = get_value_from_tree(["y", "_"], data, [])
                    self.objects.add_object(ObjMapSpriteObject(make_sprite(config.datadir + image),
                                                               CL_Point(x, y),
                                                               make_metadata(BadGuy(name))).to_object())
                else:
                    print "Error: Couldn't resolve object type: ", name
                    print "Sector: Unhandled tag: ", name

        if (self.background == None):
            self.background = TilemapLayer(tileset, width, height)

        if (self.interactive == None):
            self.interactive = TilemapLayer(tileset, width, height)

        if (self.foreground == None):
            self.foreground = TilemapLayer(tileset, width, height)

        self.editormap = EditorMap()
        self.editormap.add_layer(self.background.to_layer())
        self.editormap.add_layer(self.interactive.to_layer())
        self.editormap.add_layer(self.foreground.to_layer())
        self.editormap.add_layer(self.objects.to_layer())
        
        self.editormap.set_metadata(make_metadata(self))

    def activate(self, workspace):
        workspace.set_map(self.editormap)
        TilemapLayer.set_current(self.interactive)
        ObjectLayer.set_current(self.objects)
        connect(self.editormap.sig_change(), on_map_change)
        
# EOF #
