##  $Id: Makefile.am,v 1.4 2003/10/10 21:06:22 grumbel Exp $
## 
##  Windstille
##  Copyright (C) 2003 Ingo Ruhnke <grumbel@gmx.de>
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

command_source = \
  editor.hxx editor.cxx \
  game.hxx   game.cxx \
  gui.hxx    gui.cxx

noinst_LIBRARIES = libwindstille_scripting.a

libwindstille_scripting_a_CPPFLAGS = \
  @WINDSTILLE_CFLAGS@

libwindstille_scripting_a_SOURCES = \
  windstille_wrap.cxx \
  $(command_source)

# Include windstille_wrap.cxx in case the user doesn't have swig installed
EXTRA_DIST = windstille_wrap.cxx windstille.i

windstille_wrap.cxx: windstille.i $(command_source)
	$(SWIG) -guile -c++  -o $@ $<

# We override the default compile command to skip all the warnings
# that compiling the wrappers causes.
libwindstille_scripting_a-windstille_wrap.o : windstille_wrap.cxx
	$(CXX) -c -O2 -o $@ $<

# EOF #
