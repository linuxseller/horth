#!/bin/env bash
set -xe
if [[ $1 == compile ]] then
    as test.asm -g -o test.o
    ld test.o -o test
    rm test.o
    exit
fi
SRC_FILES="./src/Main.hs ./src/Parser.hs ./src/Runner.hs ./src/Data/Memory.hs ./src/Data/Parser.hs ./src/Compiler.hs"

ghc $SRC_FILES -isrc/Data -isrc -outputdir ./build -o ./build/horth.o
#rm src/Parser.hi src/Parser.o src/Main.hi src/Main.o
if [[ $1 == run ]] then
    ./build/horth.o
fi

