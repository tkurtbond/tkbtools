#! /usr/bin/env bash

print_vars () {
    local maxlen
    for var in "$@"
    do
	((maxlen < ${#var})) && ((maxlen=${#var}))
    done
    for var in "$@"
    do
	printf '%-*s %s\n' $maxlen "$var" "${!var}"
    done
}

(($# > 0)) && print_vars "$@"
