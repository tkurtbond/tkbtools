#! /usr/bin/env python

import sys, string

if len (sys.argv) <= 1:
    sys.stderr.write ("""usage: dedos file ...
    rewrites files without carriage returns, in place\n""")
    sys.exit (2)

for filename in sys.argv[1:]:
    file = open (filename, "r")
    text = file.read ()
    file.close ()
    text = string.replace (text, "\r", "")
    file = open (filename, "w")
    file.write (text)
    file.close ()
