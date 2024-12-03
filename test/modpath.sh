#! /usr/bin/env bash

for i in Src/modpath Src/Scheme/Chicken/build/smodpath Src/Ada/bin/amodpath
do
    echo modpath variant $i
    MODPATH=$i ./test/bats/bin/bats test/modpath.bats || exit
done
