#!/bin/sh

rm -f Makefile libflexlay/Makefile simple/Makefile
rm -f libflexlay/moc_*.cpp
qmake flexlay.pro

# EOF #
