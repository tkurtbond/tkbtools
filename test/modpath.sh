#! /usr/bin/env bash

EXIT_ON_FAILURE=1
MODPATH=''

while getopts "aems" opt
do
    case "$opt" in
        (\?) ((errors++)) ;;
        (a) MODPATH=amodpath ;;
        (e) EXIT_ON_FAILURE=0 ;;
        (m) MODPATH=modpath ;;
        (s) MODPATH=smodpath;;
    esac
done


for i in Src/modpath Src/Scheme/Chicken/build/smodpath Src/Ada/bin/amodpath
do
    if [[ -n $MODPATH ]] && ! [[ $i =~ .*/$MODPATH ]]; then
        echo skipping variant $i
        continue
    fi
    echo modpath variant $i
    MODPATH=$i ./test/bats/bin/bats test/modpath.bats
    EXIT_STATUS=$?
    if ((EXIT_ON_FAILURE)) && ((EXIT_STATUS)); then
        exit
    fi
done
