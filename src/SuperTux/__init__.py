class BadGuy:
    type = None
 
    def __init__(self,  type):
        self.type = type

execfile("SuperTux/config.py")

config  = Config()

print "Config ready: ", config

execfile("SuperTux/tileset.py")
execfile("SuperTux/gui.py")
execfile("SuperTux/shell.py")
execfile("SuperTux/sector.py")
execfile("SuperTux/level.py")
execfile("SuperTux/display_properties.py")

tileset = Tileset(32)
tileset.load("/home/ingo/cvs/supertux/supertux/data/images/tilesets/supertux.stgt")

    
game_objects = [["money", "images/shared/jumpy-left-middle-0.png"],
                ["snowball", "images/shared/snowball-left-0.png"],
                ["mriceblock", "images/shared/mriceblock-left-0.png"],
                ["mrbomb", "images/shared/mrbomb-left-0.png"],
                ["flame", "images/shared/flame-0.png"], 
                ["stalactite", "images/shared/stalactite.png"],
                ["fish", "images/shared/fish-left-0.png"],
                ["flyingsnowball", "images/shared/flyingsnowball-left-0.png"],
                ["bouncingsnowball", "images/shared/bouncingsnowball-left-0.png"],
                ["spiky", "images/shared/spiky-left-0.png"],
                ["resetpoint", "images/shared/resetpoint.png"],
                ["playerspawn", "images/shared/resetpoint.png"],
                ["door", "images/shared/door.png"],
                ["trampoline", "images/shared/trampoline-1.png"]
                ]

# EOF #
