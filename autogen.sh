#!/bin/sh

aclocal-1.7
autoheader
automake-1.7 -f --copy --add-missing
autoconf

# EOF #
