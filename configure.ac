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
  TERMINFO_LIB_DIRS=$curses_libraries
fi
if test "x$curses_includes" != "xNONE"; then
  TERMINFO_INCLUDE_DIRS=$curses_includes
fi
AC_SUBST(TERMINFO_INCLUDE_DIRS)
AC_SUBST(TERMINFO_LIB_DIRS)


AC_CONFIG_FILES([terminfo.buildinfo])
AC_OUTPUT
