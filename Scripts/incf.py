#! /usr/bin/env python

# I should turn this into a python function.  Need to decide on personal python
# module namespace first.

import time
from optparse import OptionParser
import os.path

version = "python 0.1"
usage = 'usage: %prog [options] prefix [suffix]'
parser = OptionParser (usage=usage, version='%prog ' + version)
parser.add_option ('-v', '--verbose',
                   action='store_true', dest='verbose', default=False,
                   help='make lots of noise')

sep1 = '_'
sep2 = '_'

def main ():
    (options, args) = parser.parse_args ()
    nargs = len (args)
    if nargs == 1:
        prefix = args[0]
        suffix = ''
    elif nargs == 2:
        prefix = args[0]
        suffix = args[1]
    else:
        parser.error('incorrect number of arguments')
    date = time.strftime ('%F')
    fileprefix = prefix + sep1 + date
    i = 0
    # The zeroth filename doesn't have the number.
    testname = fileprefix + suffix
    while True:
        if not os.path.lexists (testname):
            break
        i += 1
        testname = fileprefix + sep2 + str (i) + suffix
    print testname


if __name__ == '__main__':
    main()
