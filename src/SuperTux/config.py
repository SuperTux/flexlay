import os
import ConfigParser

class Config:
    config = None
    datadir = None
    recent_files = []
    
    def __init__(self):
        self.config = ConfigParser.ConfigParser()

        # Setting defaults
        self.config.add_section("SuperTux")
        self.config.set("SuperTux", "datadir", "/home/ingo/cvs/supertux/supertux/data/")
        self.config.set("SuperTux", "recent_files", [])

        self.config.read(['supertux.cfg', os.path.expanduser('~/.flexlay/supertux.cfg')])

        self.datadir      = self.config.get("SuperTux", "datadir")
        str = self.config.get("SuperTux", "recent_files")
        print str
        self.recent_files = eval(str)
        
    def __del__(self):
        self.config.set("SuperTux", "datadir", self.datadir)
        self.config.set("SuperTux", "recent_files", self.recent_files)
        
        self.config.write(open(os.path.expanduser('~/.flexlay/supertux.cfg'), 'w'))

# EOF #
