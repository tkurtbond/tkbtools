#! /usr/bin/env python
# usage:
#   (cd ~/myblog/entries && find . -type f -name \*.rst) |
#   sed 's#^\./##' |
#   (cd /sw/src/tinkerer/tinktest && ~/comp/tkbtools/Scripts/totinkerer.py)

import os.path, re, string, sys

datetime_re = re.compile ('^#published[ \t](.+)$')
date_re = re.compile ('([0-9]{4}-[0-9]{2}-[0-9]{2})')
tags_re = re.compile ('^#tags[ \t](.+)$')

def main ():
    for infilename in sys.stdin:
        with open (infilename, 'r') as infile:
            title = infile.readline ()
            title = title.rstrip ()
            datetime_match = re.match (datetime_re, infile.readline ())
            tags_match = re.match (tags_re, infile.readline ())

            categories = os.path.dirname (infilename)
            tags = tags_match.group (1)

            date_match = re.match (date_re, datetime_match)
            date_elements = date_match.group (1).split ('-')
            dirname = '/'.join (date_elements)
            basename = os.path.basename (infile)
            outfilename = os.path.join (dirname, basename)
            
            with open (outfilename, 'w') as outfile:
                # write title and tags
                outfile.write (title)
                outfile.write ('\n')
                outfile.write ('=' * len (title))
                outfile.write ('\n')
                outfile.write ('\n')
                outfile.write ('.. author:: default\n')
                outfile.write ('.. categories:: ' + categories + '\n')
                outfile.write ('.. tags:: ' + tags + '\n')

                for line in infile:
                    outfile.write (infile.readline ())

