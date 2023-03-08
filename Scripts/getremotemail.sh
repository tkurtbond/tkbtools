#!/bin/sh
DEBUG=
user=$1
site=$2

if [ -z "$user" -o -z "$site" ]
then 
    echo usage: $0 user sitename
    exit 2
fi
prefix="$user-$site-`date +%Y-%m-%d.`"
filename=`increment -- $prefix .mail`
echo $filename
ssh $user@$site $DEBUG' ls -l $MAIL && '$DEBUG' [ -s $MAIL ] && '$DEBUG' movemail $MAIL $HOME/pending/'$filename' && '$DEBUG' ls -l $MAIL && echo non-zero file || (echo zero file or error; false)' && $DEBUG scp $user@$site:pending/$filename $HOME/pending/$filename && $DEBUG ls -l $HOME/pending/$filename
