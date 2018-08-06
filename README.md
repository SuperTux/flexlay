Flexlay
=======

Flexlay is a generic 2d editor framework with special focus on games.
It is written in Python3 and uses Qt5. It currently supports multi
layered tile-, object- and bitmaps, full undo/redo, support for
tile-brushes, easy copy/paste, multiple buffers, minimap support, a
metadata editor and some other stuff usefull for creating levels for
2d games.

Supported games:

* [SuperTux](http://www.supertux.org/)
* [netPanzer](https://sourceforge.net/projects/netpanzer.berlios/) (currently defunct in `master`, see `flexlay-clanlib-master` tag)

Since Flexlay is a framework and not an editor itself, it has to be
started via a game specific script (e.g. supertux-editor,
netpanzer-editor).

To run the SuperTux version of Flexlay, see [the
wiki](https://github.com/SuperTux/flexlay/wiki/Installing-the-Flexlay-SuperTux-editor).

Flexlay is covered under the GNU GPL, which means that you can copy
and even modify it pretty much as you like, as long as you keep the
copyright headers in place and distribute the source too if you
distribute binaries, see the file COPYING for details.
