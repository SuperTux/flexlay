##  $Id: Makefile.am,v 1.6 2003/08/18 08:50:22 grumbel Exp $
## 
##  Flexlay - A Generic 2D Game Editor
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

SUBDIRS = src data

EXTRA_DIST = TODO

dist-binary: distdir
	cd src && $(MAKE) windstille.static
	$(mkinstalldirs) $(distdir)/share/guile/ $(distdir)/lib/
	cp -R /usr/share/guile/1.6/ice-9/ $(distdir)/share/guile/
	cp -R /usr/share/guile/1.6/oop/   $(distdir)/share/guile/
	cp -R /usr/share/guile/1.6/srfi/  $(distdir)/share/guile/
	cp /lib/libgcc_s.so.1 /lib/libc.so.6 /lib/libpthread.so.0 /lib/ld-linux.so.2  $(distdir)/lib/
	cp src/windstille.static   $(distdir)/src/
	cp contrib/windstille.sh   $(distdir)/windstille
	mv $(distdir) $(distdir)-binary-i386
	$(AMTAR) chof - $(distdir)-binary-i386 | bzip2 -9 -c >$(distdir)-binary-i386.tar.bz2
	mv $(distdir)-binary-i386 $(distdir)
	$(am__remove_distdir)

# EOF #
