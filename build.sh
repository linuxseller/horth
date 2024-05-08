#!/bin/env bash
set -xe

ghc ./src/Main.hs ./src/Parser.hs -outputdir ./build -o ./build/horth
#rm src/Parser.hi src/Parser.o src/Main.hi src/Main.o
if [[ $1 == run ]] then
    ./build/horth
fi

