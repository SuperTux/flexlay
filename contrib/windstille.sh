#!/bin/sh

cd src/
GUILE_LOAD_PATH=../share/guile/
export GUILE_LOAD_PATH
exec ./windstille.static "$@"

# EOF #
