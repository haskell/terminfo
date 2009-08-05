AC_INIT([Haskell terminfo package], [0.2], [judah dot jacobson at gmail dot com], [terminfo])

AC_ARG_WITH([curses-includes],
  [AC_HELP_STRING([--with-curses-includes],
    [directory containing curses.h])],
    [curses_includes=$withval],
    [curses_includes=NONE])

AC_ARG_WITH([curses-libraries],
  [AC_HELP_STRING([--with-curses-libraries],
    [directory containing curses library])],
    [curses_libraries=$withval],
    [curses_libraries=NONE])


TERMINFO_INCLUDE_DIRS=
TERMINFO_LIB_DIRS=
if test "x$curses_libraries" != "xNONE"; then
  LDFLAGS="-L$curses_libraries $LDFLAGS"
  TERMINFO_LIB_DIRS=$curses_libraries
fi
if test "x$curses_includes" != "xNONE"; then
  CPPFLAGS="-I$curses_includes $CPPFLAGS"
  TERMINFO_INCLUDE_DIRS=$curses_includes
fi

AC_ARG_WITH([cc],
            [C compiler],
            [CC=$withval])
AC_PROG_CC()

AC_CHECK_HEADER(ncurses.h, CursesIncludes='ncurses.h',
    [AC_CHECK_HEADER(curses.h, CursesIncludes='curses.h', HaveCursesH=NO)])

# on Solaris, curses.h must be imported before term.h.
if test "x$HaveCursesH" = xNO ; then
    AC_MSG_FAILURE([curses headers could not be found, so this package cannot be built])
else
    TERMINFO_INCLUDES="$CursesIncludes term.h"
fi

AC_CHECK_LIB(ncursesw, setupterm, HaveLibCurses=YES; LibCurses=ncursesw,
  [AC_CHECK_LIB(ncurses, setupterm, HaveLibCurses=YES; LibCurses=ncurses,
    [AC_CHECK_LIB(curses, setupterm, HaveLibCurses=YES; LibCurses=curses,
      HaveLibCurses=NO; LibCurses=not-installed)])])

if test "x$HaveLibCurses" = "xNO" ; then
    AC_MSG_FAILURE([curses library not found, so this package cannot be built])
else
    TERMINFO_LIB=$LibCurses
fi


AC_SUBST(TERMINFO_INCLUDES)
AC_SUBST(TERMINFO_INCLUDE_DIRS)
AC_SUBST(TERMINFO_LIB_DIRS)
AC_SUBST(TERMINFO_LIB)


AC_CONFIG_FILES([terminfo.buildinfo])
AC_OUTPUT
