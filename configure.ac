AC_INIT(Windstille, 0.2.0)
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

REQUIRED_CLANLIB_VERSION="0.7.7"

CXXFLAGS="$CXXFLAGS -I\${top_srcdir}/src/"

AC_MSG_CHECKING(for debug mode)
AC_ARG_ENABLE(debug, AC_HELP_STRING([--enable-debug], [Enable debugging support (default: yes)]),, enable_debug="yes")
if test "$enable_debug" != "no"; then
    CXXFLAGS="$CXXFLAGS -Wall -O0 -g"
    AC_MSG_RESULT([enabled])
else
    AC_MSG_RESULT([disabled])
fi

AC_MSG_CHECKING(for maintainer mode)
AC_ARG_ENABLE(debug, AC_HELP_STRING([--enable-maintainer-mode], [Enable maintainer mode (default: no)]),, enable_maintainer_mode="no")
if test "$enable_maintainer_mode" != "no"; then
    CXXFLAGS="$CXXFLAGS -Werror"
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
  clanCore-0.7           >= $REQUIRED_CLANLIB_VERSION
  clanApp-0.7            >= $REQUIRED_CLANLIB_VERSION
  clanDisplay-0.7        >= $REQUIRED_CLANLIB_VERSION
  clanGL-0.7             >= $REQUIRED_CLANLIB_VERSION
  clanGUI-0.7            >= $REQUIRED_CLANLIB_VERSION
  clanGUIStyleSilver-0.7 >= $REQUIRED_CLANLIB_VERSION
dnl clanSound   >= $REQUIRED_CLANLIB_VERSION
dnl clanMikMod  >= $REQUIRED_CLANLIB_VERSION
  clanSound-0.7  >= $REQUIRED_CLANLIB_VERSION
  clanVorbis-0.7  >= $REQUIRED_CLANLIB_VERSION
  ],
 [])

AC_SUBST(WINDSTILLE_CFLAGS)
AC_SUBST(WINDSTILLE_LIBS)

INPUT_CFLAGS=$WINDSTILLE_CFLAGS
INPUT_LIBS=$WINDSTILLE_LIBS
AC_SUBST(INPUT_CFLAGS)
AC_SUBST(INPUT_LIBS)

PKG_CHECK_MODULES(EDITOR,
 [
  clanCore-0.7           >= $REQUIRED_CLANLIB_VERSION
  clanApp-0.7            >= $REQUIRED_CLANLIB_VERSION
  clanDisplay-0.7        >= $REQUIRED_CLANLIB_VERSION
  clanGL-0.7             >= $REQUIRED_CLANLIB_VERSION
  clanSDL-0.7             >= $REQUIRED_CLANLIB_VERSION
  clanGUI-0.7            >= $REQUIRED_CLANLIB_VERSION
  clanGUIStyleSilver-0.7 >= $REQUIRED_CLANLIB_VERSION
  ],
 [])
AC_SUBST(EDITOR_CFLAGS)
AC_SUBST(EDITOR_LIBS)

AC_SUBST(EDITOR_CFLAGS)
AC_SUBST(EDITOR_LIBS)

AC_CONFIG_FILES( 
    Makefile
    data/Makefile
    src/Makefile
    src/collision/Makefile
    src/scripting/Makefile
    src/input/Makefile
    src/guile/Makefile
    src/guistyle/Makefile
    src/editor/Makefile
    src/editor/scripting/Makefile
)
AC_OUTPUT

dnl Local Variables:
dnl compile-command: "./autogen.sh && ./configure"
dnl End:

dnl EOF dnl
