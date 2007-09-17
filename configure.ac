AC_INIT([Haskell terminfo package], [0.0], [judah.jacobson@gmail.com], [terminfo])

AC_PROG_CC
AC_CHECK_LIB(ncurses, tigetflag, LibTerminfo=ncurses,
    AC_CHECK_LIB(curses, tigetflag, LibTerminfo=curses,
        AC_CHECK_LIB(terminfo, tigetflag, LibTerminfo=terminfo,
            AC_CHECK_LIB(tinfo, tigetflag, LibTerminfo=tinfo,
                LibTerminfo=NONE))))

if test $LibTerminfo = NONE ; then
    AC_MSG_FAILURE([terminfo library not found, so this package cannot be
                    built.])
else
    TERMINFO_LIB=$LibTerminfo
fi

AC_SUBST([TERMINFO_LIB])
    
AC_CONFIG_FILES([terminfo.buildinfo])
AC_OUTPUT
