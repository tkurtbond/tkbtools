#! /usr/bin/env bash

[[ -z "${GOROOT}" ]] && {
    echo "\$GOROOT undefined or zero length, exiting";
    exit 1
}
    

cd ${GOROOT}/src
hg pull
hg update release
./all.bash
