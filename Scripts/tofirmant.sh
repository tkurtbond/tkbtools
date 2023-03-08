#! /usr/bin/env bash

equals="================================================================================"

((fileno = 0))
find $1 -type f -name \*.rst ! -name YYYY\* -print |
while read fn; do
    ((fileno++))
    published="$(grep '^#published' $fn)"
    date="$(echo $published | cut -d' ' -f2)"
    time="$(echo $published | cut -d' ' -f3)"
    tags="$(grep '^#tags' $fn | sed 's/^#tags[ 	][ 	]*//')"
    echo $fileno: $fn: published: $published date: $date time: $time tags: $tags
    ((n = 1))
    newname="$date-$(basename $fn)"
    while [ -r $newname ]; do
	((n++))
	newname="$date-$(basename $fn .rst)-${n}.rst"
    done
    echo newname: $newname
    (
	echo "$equals"
	head -1 $fn
	echo "$equals"
	echo
	[ -n "$time" ] && echo ".. time:: $time"
	echo ".. timezone:: EST"
	echo ".. author:: T. Kurt Bond"
	echo ".. feed:: all"
	echo "$tags" | tr ',' '\n' |
	while read tag; do 
	    echo ".. feed:: $tag"
	    echo >../feeds/$tag.rst "$tag"
	    echo >>../feeds/$tag.rst "${equals:0:${#tag}}"
	done
	echo
	tail -n +2 $fn
    ) >$newname 
done
