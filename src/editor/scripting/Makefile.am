##  $Id: Makefile.am,v 1.4 2003/10/10 21:06:22 grumbel Exp $
## 
##  Editor
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
  gui.hxx    gui.cxx \
  netpanzer.hxx netpanzer.cxx

noinst_LIBRARIES = libeditor_scripting.a

libeditor_scripting_a_CPPFLAGS = \
  -I.. @EDITOR_CFLAGS@

libeditor_scripting_a_SOURCES = \
  editor_wrap.cxx \
  $(command_source)

# Include editor_wrap.cxx in case the user doesn't have swig installed
EXTRA_DIST = editor_wrap.cxx editor.i

editor_wrap.cxx: editor.i $(command_source)
	$(SWIG) -guile -c++  -o $@ $<

# We override the default compile command to skip all the warnings
# that compiling the wrappers causes.
libeditor_scripting_a-editor_wrap.o : editor_wrap.cxx
	$(CXX) @EDITOR_CFLAGS@ -c -O2 -o $@ $<

# EOF #
