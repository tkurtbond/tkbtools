#! /usr/bin/env bash
RST2HTML=${RST2HTML:-rst2html-2.6.py}

find $1 -type f -iname \*.rst | 
while read pathname 
do
    dn="$(dirname $pathname)"
    fn="$(basename $pathname)"
    bn="$(basename $fn .rst)"
    hn="$bn.html"
    hp="$dn/$hn"
    if [[ ! -a $hp ]] || [[ $pathname -nt $hp ]]
    then
	echo $pathname
	grep -F -v \
	    -e ".. published::" -e ".. time::" \
	    -e ".. timezone::" -e ".. author::" -e ".. tags::" $pathname | \
	$RST2HTML --initial-header-level=3 --strip-comments --no-generator \
	    - $hp
    fi
    published=$(grep -F -e ".. published::" | sed s'/\.\. published::[ 	]+//')
done
