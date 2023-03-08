#!/bin/sh

found=0
path=$(IFS=:; echo $PATH)
for name in "$@"
do
    for ext in "" .exe 
    do
	for dir in $path
	do
	    pathname="$dir/$name$ext"
	    if [ -r "$pathname" ]
	    then
		found=1
		echo "$pathname"
	    fi
	done
    done
done
