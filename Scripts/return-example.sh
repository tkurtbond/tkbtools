#! /usr/bin/env bash

set -x 

try () {
    RET="$1"
}

try one
echo one: $RET
try two 
echo two: $RET
