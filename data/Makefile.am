##  $Id: Makefile.am,v 1.6 2003/12/02 16:50:41 grumbel Exp $
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

EXTRA_DIST = \
  windstille.xml \
  tiles.scm \
  tiles.xml \
  game.scm \
  editor.scm \
  gui/gui.xml \
  gui/*/*.tga \
  gui/*/*.png \
  $(wildcard music/*.ogg) \
  $(wildcard levels/*.scm) \
  $(wildcard controller/*.scm) \
  $(wildcard *.scm) \
  $(wildcard images/*.png images/*.jpg) \
  $(wildcard images/tiles/*.png) \
  $(wildcard images/bonus/*.jpg) \
  $(wildcard images/diamond/*.png) \
  $(wildcard images/hero_run/*.png) \
  $(wildcard images/hero_dead/*.png)

nobase_pkgdata_DATA = $(EXTRA_DIST)

tiles.xml:
	echo "<resources>" >  $@
	echo "  <section name=\"tiles\">" >> $@
	for i in images/tiles/*.png; do \
		echo "    <sprite name=\""`basename $${i%%.png}`"\">"; \
		echo "      <image file=\"$$i\" />"; \
		echo "    </sprite>"; \
	done  >> $@;
	echo "  </section>" >> $@
	echo "</resources>" >> $@

.PHONY: tiles.xml

# EOF #
