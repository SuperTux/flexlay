# -*- python -*-

import os

# Set to False to use local ClanLib instead of global one
if True:
    clanLib_env = Environment(ENV=os.environ)
    clanLib_env.ParseConfig("pkg-config --cflags --libs " +
                            "clanCore-1.0 clanDisplay-1.0 clanGL-1.0 clanSignals-1.0 clanGUI-1.0 clanGUIStyleSilver-1.0")
else:
    # FIXME: replace the X11 stuff with a proper X11 configure check and
    # make them somehow part of the clanlib libraries themself
    clanLib_env = Environment(CPPPATH=['../external/clanlib/'],
                              LIBPATH=['/usr/X11R6/lib/',
                                       '../external/clanlib/'],
                              LIBS=['clanGUIStyleSilver',
                                    'clanGUI',
                                    'clanGL',
                                    'clanDisplay',
                                    'clanSignals',
                                    'clanCore',
                                    'X11', 'Xmu', 'GL', 'GLU', 'png', 'jpeg', 'Xxf86vm', 'Xi'])

Export('clanLib_env')

# SConscript(['external/clanlib/SConstruct'])
SConscript(['lib/SConscript'])
SConscript(['ruby/SConscript'])
SConscript(['netpanzer/SConscript'])

# EOF #
