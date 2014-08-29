#!/bin/sh

for i in "$@"; do
    echo $i
    NAME=$(echo $i | sed "s/[\/\.]/_/g;s/^/HEADER_FLEXLAY_/" | tr [a-z] [A-Z])
    sed  -i "s/^#ifndef .*_HXX\$/#ifndef $NAME/" $i; 
    sed  -i "s/^#define .*_HXX\$/#define $NAME/" $i; 
done

# EOF #
