#!/bin/env bash
set -xe

SRC_FILES="./src/Main.hs ./src/Parser.hs ./src/Runner.hs ./src/Data/Memory.hs"

ghc $SRC_FILES -isrc/Data -isrc -outputdir ./build -o ./build/horth
#rm src/Parser.hi src/Parser.o src/Main.hi src/Main.o
if [[ $1 == run ]] then
    ./build/horth
fi

