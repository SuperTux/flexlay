#!/bin/sh

aclocal-1.7
automake-1.7 -f --copy --add-missing
autoheader
autoconf

# EOF #