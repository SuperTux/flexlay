AC_INIT(Windstille, 0.1.1)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([src/windstille_main.cxx])
AM_INIT_AUTOMAKE(dist-bzip2)

AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_LANG(C++)

dnl Checking for libraries
AC_CHECK_LIB(qthreads, main)
AC_CHECK_LIB(guile, scm_boot_guile)
AC_CHECK_HEADER(guile/gh.h)

REQUIRED_CLANLIB_VERSION="0.7.3"

AC_MSG_CHECKING(for debug mode)
AC_ARG_ENABLE(debug, AC_HELP_STRING([--enable-debug], [Enable debugging support (default: yes)]),, enable_debug="yes")
if test "$enable_debug" != "no"; then
    CXXFLAGS="$CXXFLAGS -Wall -Werror -O0 -g"
    AC_MSG_RESULT([enabled])
else
    AC_MSG_RESULT([disabled])
fi

AC_PATH_PROG([SWIG], [swig], [nothing-found])
if test "x$SWIG" = "xnothing-found"; then
   AC_MSG_ERROR([You need Swig - http://www.swig.org to proceed.])
fi

PKG_CHECK_MODULES(WINDSTILLE,
 [
  clanCore    >= $REQUIRED_CLANLIB_VERSION
  clanApp     >= $REQUIRED_CLANLIB_VERSION
  clanDisplay >= $REQUIRED_CLANLIB_VERSION
  clanGL      >= $REQUIRED_CLANLIB_VERSION
  clanGUI     >= $REQUIRED_CLANLIB_VERSION
  clanGUIStyleSilver >= $REQUIRED_CLANLIB_VERSION
dnl clanSound   >= $REQUIRED_CLANLIB_VERSION
dnl clanMikMod  >= $REQUIRED_CLANLIB_VERSION
dnl clanVorbis  >= $REQUIRED_CLANLIB_VERSION
  ],
 [])

AC_SUBST(WINDSTILLE_CFLAGS)
AC_SUBST(WINDSTILLE_LIBS)

AC_CONFIG_FILES( 
    Makefile
    data/Makefile
    src/Makefile
    src/collision/Makefile
    src/guile/Makefile
    src/editor/Makefile
)
AC_OUTPUT

dnl Local Variables:
dnl compile-command: "./autogen.sh && ./configure"
dnl End:

dnl EOF dnl
