#!/bin/sh

cd src/

GUILE_LOAD_PATH=../share/guile/
export GUILE_LOAD_PATH

LD_LIBRARY_PATH=../lib/
export LD_LIBRARY_PATH

exec -a windstille ../lib/ld-linux.so.2 ./windstille.static "$@"

# EOF #
