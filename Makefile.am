##  $Id: Makefile.am,v 1.5 2003/08/13 13:49:32 grumbel Exp $
## 
##  Windstille - A free Lemmings clone
##  Copyright (C) 2002 Ingo Ruhnke <grumbel@gmx.de>
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

SUBDIRS = src data

EXTRA_DIST = INSTALL.configure

dist-binary: distdir
	cd src && $(MAKE) windstille.static
	$(mkinstalldirs) $(distdir)/share/guile/
	cp -R /usr/share/guile/1.6/ice-9/ $(distdir)/share/guile/
	cp -R /usr/share/guile/1.6/oop/   $(distdir)/share/guile/
	cp -R /usr/share/guile/1.6/srfi/  $(distdir)/share/guile/
	cp src/windstille.static   $(distdir)/src/
	cp contrib/windstille.sh   $(distdir)/windstille
	$(AMTAR) chof - $(distdir) | bzip2 -9 -c >$(distdir)-binary-i386.tar.bz2
	$(am__remove_distdir)

# EOF #
