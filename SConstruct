# -*- python -*-

import os

clanLib_env = Environment(ENV=os.environ)
clanLib_env.ParseConfig("pkg-config --cflags --libs " +
                        "clanCore-1.0 clanDisplay-1.0 clanGL-1.0 clanSignals-1.0 clanGUI-1.0 clanGUIStyleSilver-1.0")

Export('clanLib_env')

# SConscript(['external/clanlib/SConstruct'])
SConscript(['lib/SConscript'])
SConscript(['ruby/SConscript'])
SConscript(['netpanzer/SConscript'])

# EOF #
