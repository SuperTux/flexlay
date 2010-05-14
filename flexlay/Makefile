DESTDIR = 
PREFIX  = "/usr/local"
DATADIR = "${PREFIX}/share/netpanzer-editor"
MANDIR  = "${PREFIX}/share/man"
LIBDIR  = "${PREFIX}/lib/flexlay"
BINDIR  = "${PREFIX}/bin"


build: ruby/flexlay_wrap.so netpanzer/netpanzer_wrap.so

ruby/flexlay_wrap.so netpanzer/netpanzer_wrap.so:
	scons

clean:
	scons -c
	rm -rf .sconf_temp/
	rm -f .sconsign.dblite
	rm -f config.log

install: install-exec install-data install-man
	install -d ${DESTDIR}${BINDIR}
	echo \
	'#!/bin/sh\n'\
	'\n'\
	'FLEXLAY_DATADIR="'${DATADIR}'"\n'\
	'export FLEXLAY_DATADIR\n'\
	'\n'\
	'NETPANZER_DATADIR="/home/ingo/projects/netpanzer/netpanzer"\n'\
	'export NETPANZER_DATADIR\n'\
	'\n'\
	'NETPANZER_EDITOR_DATADIR="'${DATADIR}'"\n'\
	'export NETPANZER_EDITOR_DATADIR\n'\
	'\n'\
	'LD_LIBRARY_PATH=$LD_LIBRARY_PATH:'${LIBDIR}'\n'\
	'export LD_LIBRARY_PATH\n'\
	'\n'\
	'RUBYLIB=$$RUBYLIB:${DATADIR}:${LIBDIR}\nexport RUBYLIB\n'\
	'\n'\
	'exec ruby -w '${DATADIR}/netpanzer.rb' "$$@"\n\n'\
	'# EOF #' > ${DESTDIR}${BINDIR}/netpanzer-editor
	chmod 755 ${DESTDIR}${BINDIR}/netpanzer-editor

install-data:
	cd data/; \
	find -type f \( \
	-name "*.rb" -o \
	-name "*.png" -o \
	-name "*.tga" -o \
	-name "*.xml" \) \
	-exec install -D {} ${DESTDIR}${DATADIR}/{} \;

	cd ruby/; \
	find -type f \( \
	-name "*.rb" -o \
	-name "*.png" -o \
	-name "*.xml" \) \
	-exec install -D {} ${DESTDIR}${DATADIR}/{} \;

	cd netpanzer/; \
	find -type f \( \
	-name "*.rb" -o \
	-name "*.png" -o \
	-name "*.xml" \) \
	-exec install -D {} ${DESTDIR}${DATADIR}/{} \;

install-exec: build
	install -D netpanzer/netpanzer_wrap.so ${DESTDIR}${LIBDIR}/netpanzer_wrap.so 
	install -D ruby/flexlay_wrap.so ${DESTDIR}${LIBDIR}/flexlay_wrap.so

install-man:

.PHONY : build clean install install-exec install-data install-man

# EOF #
