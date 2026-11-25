#! /usr/bin/env bash
#
# Run the bats tests for Scripts/vtape-check.  Run from the repository root:
#     ./test/vtape-check.sh [bats options...]
# e.g. ./test/vtape-check.sh --filter corrupt
#
# Set VTAPE_CHECK to test a different copy of the script, e.g.
#     VTAPE_CHECK=~/local/bin/vtape-check ./test/vtape-check.sh

exec ./test/bats/bin/bats "$@" test/vtape-check.bats
