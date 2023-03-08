#! /usr/bin/env bash

trap exit SIGINT SIGTERM

BEGIN=2
END=254
RANGE=64.181.9

let errors=0
while getopts "b:e:r:" opt
do 
    case $opt in
	\?) let errors++ ;;
	b) BEGIN="$OPTARG" ;;
	e) END="$OPTARG" ;;
	r) RANGE="$OPTARG" ;;
    esac
done
let shift_by=OPTIND=1
shift $shift_by

if [ "$#" -ne 0 ] || [ "$errors" -gt 0 ]
then
    cat <<EOF 
usage: pingem-linux.sh [-b begin] [-e end] [-r range]
EOF
fi

for ((i = BEGIN; i <= END; i++)) {
    address=$RANGE.$i
    ping -c 1 -q $address >/dev/null
    status=$?
    { [[ $status -eq 0 ]] && echo $address replied; } || echo $address silent
}
