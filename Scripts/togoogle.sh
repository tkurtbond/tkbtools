#! /usr/bin/env bash
DBG=':'
equals="======================================================================================"

((fileno = 0))
find $1 -type f -name \*.rst ! -name YYYY\* -print |
while read fn; do
    ((fileno++))
    published="$(grep '^#published' $fn | sed 's/^#published[ 	]+//')"
    date="$(echo $published | cut -d' ' -f1)"
    time="$(echo $published | cut -d' ' -f2)"
    tags="$(grep '^#tags' $fn | sed 's/^#tags[ 	][ 	]*//')"
    $DBG echo $fileno: $fn: published: $published date: $date time: $time tags: $tags
    newname="$(echo $fn | sed 's#^.*/entries/##')"
    newdir="$(dirname $newname)"
    filename="$(basename $fn .rst)"
    inc=""
    ((n = 1))
    while true
    do
        [[ -r $newdir/$filename$inc.rst ]] || break
	inc="-$n"
	((n++))
    done
    $DBG echo newdir: $newdir
    $DBG echo newname: $newname
    dstname="$newdir/$filename$inc.rst"
    echo dstname: $dstname
    if [[ "$newname" == "$dstname" ]]
    then
	echo "$newname"
    else
	echo ">>> $newname != $dstname"
    fi
    mkdir -p $newdir
    (
	echo "$equals"
	head -1 $fn
	echo "$equals"
	echo
	echo ".. published:: $published"
	[ -n "$time" ] && echo ".. time:: $time"
	echo ".. timezone:: EST"
	echo ".. author:: T. Kurt Bond"
	echo ".. tags:: $tags"
	echo
	grep -v -e '^#published' -e "^#tags" $fn | tail -n +2 
    ) >$newname 
done
