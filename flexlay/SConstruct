# -*- python -*-

# FIXME: replace the X11 stuff with a proper X11 configure check and
# make them somehow part of the clanlib libraries themself
clanLib_env = Environment(CPPPATH=['../clanlib/'],
                          LIBPATH=['/usr/X11R6/lib/',
                                   '../clanlib/'],
                          LIBS=['clanGUIStyleSilver', 
                                'clanGUI',      
                                'clanGL',
                                'clanDisplay',
                                'clanSignals', 
                                'clanCore',
                                'X11', 'Xmu', 'GL', 'GLU', 'png', 'jpeg', 'Xxf86vm', 'Xi'])

# Use this if you want to use your globally installed ClanLib
#  clanLib_env = Environment()
#  clanLib_env.ParseConfig("pkg-config --cflags --libs clanCore-0.8 clanDisplay-0.8 clanGL-0.8 clanSignals-0.8 clanGUI-0.8 clanGUIStyleSilver-0.8")

Export('clanLib_env')

SConscript(['clanlib/SConstruct'])
SConscript(['lib/SConscript'])
SConscript(['ruby/SConscript'])
SConscript(['supertux/SConstruct'])
SConscript(['netpanzer/SConstruct'])

# EOF #

