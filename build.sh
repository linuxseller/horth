#!/bin/env bash
set -xe

ghc ./src/Main.hs -o ./build/horth
if [[ $1 == run ]] then
    ./build/horth
fi

