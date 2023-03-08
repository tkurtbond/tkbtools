#! /usr/bin/env bash

for i in "$@"
do

gawk '
BEGIN 		{
                  header = "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
		  }
/^#tags/	{ sub(/^#tags/, ":tags:"); }
/^#published/	{ sub(/^#published/, ":date:"); }
FNR == 1        { print ""; print ""; print header; }
		{ 
		    if (OLDNAME == "") OLDNAME = FILENAME;
		    if (FILENAME != OLDNAME) {
		        print ""; print header; OLDNAME = FILENAME; }
		    print; 
		}
FNR == 1 	{ print header; }
' <$i

done
