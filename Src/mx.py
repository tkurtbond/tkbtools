#! /usr/bin/python

import sys
import string
import re
import os.path

for line in sys.stdin:
    line = line.strip('\n\r\t ')
    #print line
    [filename, rest] = line.split (':', 1)
    (filename, tail) = os.path.splitext (filename)
    if rest[0] == ';': continue
    #mxhost = re.split('[ \t]+MX[ \t]+', rest)[1]
    mxhost = rest.split ()[-1]
    if mxhost[-1] != '.':
        mxhost = mxhost + '.' + filename
    else:
        mxhost = mxhost[:-1]
    print mxhost
