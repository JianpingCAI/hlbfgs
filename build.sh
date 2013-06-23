#!/bin/bash

set -e

HFLAGS="-O2 -isrc -outputdir obj"
LIBS="-llbfgs -Lflib -lgfortran -L/usr/local/opt/gfortran/gfortran/lib"

rm -rf obj

mkdir obj

gcc -O2 -c csrc/driver.c -o obj/driver.o
ghc --make $HFLAGS $LIBS tests/Test.hs obj/driver.o -o Test.x
haddock src/HLBFGS.hs -h -o doc

rm -rf obj
