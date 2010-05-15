DESTDIR = 
PREFIX  = "/usr/local"
DATADIR = "${PREFIX}/share"
MANDIR  = "${PREFIX}/share/man"
LIBDIR  = "${PREFIX}/lib/flexlay"
BINDIR  = "${PREFIX}/bin"

FLEXLAY_DATADIR          = ${DATADIR}/flexlay
NETPANZER_EDITOR_DATADIR = ${DATADIR}/netpanzer-editor
FLEXLAY_PAINT_DATADIR    = ${DATADIR}/flexlay-paint

build:
	scons

clean:
	scons -c
	rm -rf .sconf_temp/
	rm -f .sconsign.dblite
	rm -f config.log

install: build install-exec install-data

install-exec: install-flexlay-exec install-flexlay-paint-exec install-netpanzer-editor-exec
install-data: install-flexlay-data install-flexlay-paint-data install-netpanzer-editor-data

install-flexlay: install-flexlay-exec install-flexlay-data
install-flexlay-paint: install-flexlay-paint-exec install-flexlay-paint-data
install-netpanzer-editor: install-netpanzer-editor-exec install-netpanzer-editor-data

install-flexlay-exec:
	install -D ruby/flexlay_wrap.so ${DESTDIR}${LIBDIR}/flexlay_wrap.so

install-flexlay-data:
	cd data/; \
	find -type f \( \
	-name "*.rb" -o \
	-name "*.png" -o \
	-name "*.tga" -o \
	-name "*.xml" \) \
	-exec install -m 644 -D {} ${DESTDIR}${FLEXLAY_DATADIR}/{} \;

	cd ruby/; \
	find -type f \( \
	-name "*.rb" -o \
	-name "*.png" -o \
	-name "*.xml" \) \
	-exec install -m 644 -D {} ${DESTDIR}${FLEXLAY_DATADIR}/{} \;

install-netpanzer-editor-exec:
	install -D netpanzer/netpanzer_wrap.so ${DESTDIR}${LIBDIR}/netpanzer_wrap.so 

	install -d ${DESTDIR}${BINDIR}
	echo \
	'#!/bin/sh\n'\
	'\n'\
	'FLEXLAY_DATADIR="'${FLEXLAY_DATADIR}'"\n'\
	'export FLEXLAY_DATADIR\n'\
	'\n'\
	'NETPANZER_DATADIR="/usr/share/games/netpanzer/"\n'\
	'export NETPANZER_DATADIR\n'\
	'\n'\
	'NETPANZER_EDITOR_DATADIR="'${NETPANZER_EDITOR_DATADIR}'"\n'\
	'export NETPANZER_EDITOR_DATADIR\n'\
	'\n'\
	'LD_LIBRARY_PATH=$LD_LIBRARY_PATH:'${LIBDIR}'\n'\
	'export LD_LIBRARY_PATH\n'\
	'\n'\
	'RUBYLIB="$$RUBYLIB:'${FLEXLAY_DATADIR}':'${LIBDIR}':'${NETPANZER_EDITOR_DATADIR}'"\nexport RUBYLIB\n'\
	'\n'\
	'exec ruby -w '${NETPANZER_EDITOR_DATADIR}/netpanzer.rb' "$$@"\n\n'\
	'# EOF #' > ${DESTDIR}${BINDIR}/netpanzer-editor
	chmod 755 ${DESTDIR}${BINDIR}/netpanzer-editor

install-netpanzer-editor-data:
	cd netpanzer/; \
	find -type f \( \
	-name "*.rb" -o \
	-name "*.png" -o \
	-name "*.xml" \) \
	-exec install -m 644 -D {} ${DESTDIR}${NETPANZER_EDITOR_DATADIR}/{} \;

install-flexlay-paint-exec:
	install -d ${DESTDIR}${BINDIR}
	echo \
	'#!/bin/sh\n'\
	'\n'\
	'FLEXLAY_DATADIR="'${FLEXLAY_DATADIR}'"\n'\
	'export FLEXLAY_DATADIR\n'\
	'\n'\
	'FLEXLAY_PAINT_DATADIR="'${FLEXLAY_PAINT_DATADIR}'"\n'\
	'export FLEXLAY_PAINT_DATADIR\n'\
	'\n'\
	'LD_LIBRARY_PATH=$LD_LIBRARY_PATH:'${LIBDIR}'\n'\
	'export LD_LIBRARY_PATH\n'\
	'\n'\
	'RUBYLIB="$$RUBYLIB:'${FLEXLAY_DATADIR}':'${LIBDIR}':'${FLEXLAY_PAINT_DATADIR}'"\nexport RUBYLIB\n'\
	'\n'\
	'exec ruby -w '${FLEXLAY_PAINT_DATADIR}/paint.rb' "$$@"\n\n'\
	'# EOF #' > ${DESTDIR}${BINDIR}/flexlay-paint
	chmod 755 ${DESTDIR}${BINDIR}/flexlay-paint

install-flexlay-paint-data:
	cd paint/; \
	find -type f \( \
	-name "*.rb" \) \
	-exec install -m 644 -D {} ${DESTDIR}${FLEXLAY_PAINT_DATADIR}/{} \;

.PHONY : build clean \
 install install-exec install-data \
 install-flexlay \
 install-flexlay-paint \
 install-netpanzer-editor \
 install-flexlay-exec          install-flexlay-data \
 install-netpanzer-editor-exec install-netpanzer-editor-data \
 install-flexlay-paint-exec    install-flexlay-paint-data

# EOF #
